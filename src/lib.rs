//! A decoder for WebAssembly modules in the binary format.
//!
//! This library implements the Binary format of the WebAssembly [specification],
//! version 2.
//!
//! The main entry point is the [`decode()`] function.
//!
//! [specification]: https://www.w3.org/TR/wasm-core-2/
#![forbid(unsafe_code)]

pub mod decode;
pub mod indices;
pub mod instructions;
pub mod integer;
pub mod types;

use crate::decode::FromMarkerByte;
use crate::decode::sections::*;
use crate::decode::sections::{
    custom::CustomSection, data::Data, element::Elem, export::Export, function::Func,
    global::Global, import::Import,
};
use crate::indices::FuncIdx;
use crate::instructions::Instruction;
use crate::types::{functype::FuncType, memtype::MemType, tabletype::TableType};

use integer::*;
use phf::phf_ordered_map;
use std::io;
use std::io::Read;
use thiserror::Error;

const MAGIC_NUMBER: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];
const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

static EXPECTED_PREAMBLE: [u8; 8] = [
    MAGIC_NUMBER[0],
    MAGIC_NUMBER[1],
    MAGIC_NUMBER[2],
    MAGIC_NUMBER[3],
    VERSION[0],
    VERSION[1],
    VERSION[2],
    VERSION[3],
];

/// WebAssembly programs are organized into modules, which are the unit of deployment,
/// loading, and compilation. A module collects definitions for types, functions, tables,
/// memories, and globals. In addition, it can declare imports and exports and provide
/// initialization in the form of data and element segments, or a start function.
///
/// <https://www.w3.org/TR/wasm-core-2/#modules>
/// <https://www.w3.org/TR/wasm-core-2/#binary-module>
#[derive(Debug, PartialEq)]
pub struct Module {
    pub version: [u8; 4],

    // Contains any non-custom sections encountered.
    pub parsed_section_kinds: Vec<SectionKind>,

    pub section_headers: Vec<SectionHeader>,

    /// Custom sections have the id 0. They are intended to be used for debugging information or
    /// third-party extensions, and are ignored by the WebAssembly semantics. Their contents
    /// consist of a name further identifying the custom section, followed by an uninterpreted
    /// sequence of bytes for custom use.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#custom-section>
    /// <https://www.w3.org/TR/wasm-core-2/#binary-customsec>
    pub custom_sections: Vec<CustomSection>,

    /// The types component of a module defines a vector of function types. All function types
    /// used in a module must be defined in this component. They are referenced by type indices.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#types>
    /// <https://www.w3.org/TR/wasm-core-2/#binary-typesec>
    pub types: Vec<FuncType>,

    /// The funcs component of a module defines a vector of functions with the following
    /// structure: The type of a function declares its signature by reference to a type defined
    /// in the module. The parameters of the function are referenced through 0-based local
    /// indices in the function's body; they are mutable. The locals declare a vector of mutable
    /// local variables and their types. These variables are referenced through local indices in
    /// the function's body. The index of the first local is the smallest index not referencing a
    /// parameter. The body is an instruction sequence that upon termination must produce a
    /// stack matching the function type's result type. Functions are referenced through
    /// function indices, starting with the smallest index not referencing a function import.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#functions>
    /// <https://www.w3.org/TR/wasm-core-2/#function-section>
    pub funcs: Vec<Func>,

    /// The tables component of a module defines a vector of tables described by their table
    /// type: A table is a vector of opaque values of a particular reference type. The min size
    /// in the limits of the table type specifies the initial size of that table, while its max,
    /// if present, restricts the size to which it can grow later. Tables can be initialized
    /// through element segments. Tables are referenced through table indices, starting with the
    /// smallest index not referencing a table import. Most constructs implicitly reference
    /// table index 0.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#tables>
    /// <https://www.w3.org/TR/wasm-core-2/#table-section>
    pub tables: Vec<TableType>,

    /// The mems component of a module defines a vector of linear memories (or memories for
    /// short) as described by their memory type: A memory is a vector of raw uninterpreted
    /// bytes. The min size in the limits of the memory type specifies the initial size of that
    /// memory, while its max, if present, restricts the size to which it can grow later. Both
    /// are in units of page size. Memories can be initialized through data segments. Memories
    /// are referenced through memory indices, starting with the smallest index not referencing
    /// a memory import. Most constructs implicitly reference memory index 0.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#memories>
    /// <https://www.w3.org/TR/wasm-core-2/#memory-section>
    pub mems: Vec<MemType>,

    /// The globals component of a module defines a vector of global variables (or globals for
    /// short): Each global stores a single value of the given global type. Its type also
    /// specifies whether a global is immutable or mutable. Moreover, each global is initialized
    /// with an init value given by a constant initializer expression. Globals are referenced
    /// through global indices, starting with the smallest index not referencing a global import.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#globals>
    /// <https://www.w3.org/TR/wasm-core-2/#global-section>
    pub globals: Vec<Global>,

    /// The initial contents of a table is uninitialized. Element segments can be used to
    /// initialize a subrange of a table from a static vector of elements. The elems component
    /// of a module defines a vector of element segments. Each element segment defines a
    /// reference type and a corresponding list of constant element expressions. Element
    /// segments have a mode that identifies them as either passive, active, or declarative. A
    /// passive element segment's elements can be copied to a table using the table.init
    /// instruction. An active element segment copies its elements into a table during
    /// instantiation, as specified by a table index and a constant expression defining an
    /// offset into that table. A declarative element segment is not available at runtime but
    /// merely serves to forward-declare references that are formed in code with instructions
    /// like ref.func. The offset is given by a constant expression. Element segments are
    /// referenced through element indices.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#element-segments>
    /// <https://www.w3.org/TR/wasm-core-2/#element-section>
    pub elems: Vec<Elem>,

    /// The optional data count section (id 12) declares the number of data segments that follow.
    /// It is required when data indices appear in code, and when present its count must match the
    /// length of [`Self::datas`].
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#data-count-section>
    pub data_count: Option<u32>,

    /// The initial contents of a memory are zero bytes. Data segments can be used to initialize
    /// a range of memory from a static vector of bytes. The datas component of a module
    /// defines a vector of data segments. Like element segments, data segments have a mode
    /// that identifies them as either passive or active. A passive data segment's contents can
    /// be copied into a memory using the memory.init instruction. An active data segment
    /// copies its contents into a memory during instantiation, as specified by a memory index
    /// and a constant expression defining an offset into that memory. Data segments are
    /// referenced through data indices.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#data-segments>
    /// <https://www.w3.org/TR/wasm-core-2/#data-section>
    pub datas: Vec<Data>,

    /// The start component of a module declares the function index of a start function that is
    /// automatically invoked when the module is instantiated, after tables and memories have
    /// been initialized.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#start-function>
    /// <https://www.w3.org/TR/wasm-core-2/#start-section>
    pub start: Option<FuncIdx>,

    /// The imports component of a module defines a set of imports that are required for
    /// instantiation. Each import is labeled by a two-level name space, consisting of a module
    /// name and a name for an entity within that module. Importable definitions are functions,
    /// tables, memories, and globals. Each import is specified by a descriptor with a
    /// respective type that a definition provided during instantiation is required to match.
    /// Every import defines an index in the respective index space. In each index space, the
    /// indices of imports go before the first index of any definition contained in the module
    /// itself.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#imports>
    /// <https://www.w3.org/TR/wasm-core-2/#import-section>
    pub imports: Vec<Import>,

    /// The exports component of a module defines a set of exports that become accessible to the
    /// host environment once the module has been instantiated. Each export is labeled by a
    /// unique name. Exportable definitions are functions, tables, memories, and globals, which
    /// are referenced through a respective descriptor.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#exports>
    /// <https://www.w3.org/TR/wasm-core-2/#export-section>
    pub exports: Vec<Export>,
}

impl Module {
    fn validate_section_kind_expected(
        &self,
        current: &SectionHeader,
    ) -> Result<(), DecodeModuleError> {
        let previous = self.parsed_section_kinds.last();
        let current = current.kind;

        // Every section is valid if it's the first one we're encountering.
        // Custom sections may appear anywhere in the module and multiple times
        if previous.is_none() || current == SectionKind::Custom {
            return Ok(());
        }

        let previous = *previous.unwrap();

        if current < previous {
            return Err(DecodeModuleError::SectionOutOfOrder { current, previous });
        }

        if current == previous {
            return Err(DecodeModuleError::DuplicateSection(current));
        }

        Ok(())
    }
}

impl Default for Module {
    fn default() -> Self {
        Module {
            version: VERSION,
            parsed_section_kinds: vec![],
            section_headers: vec![],
            data_count: None,
            custom_sections: vec![],
            types: vec![],
            funcs: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            elems: vec![],
            datas: vec![],
            start: None,
            imports: vec![],
            exports: vec![],
        }
    }
}

/// Each section constists of a one-byte section id, the u32 size of the contents
/// (in bytes), and the actual contents, whose structure is dependent on the section id.
///
/// Note: 'Section header' is not a term defined in the specification.
#[derive(PartialEq, Debug)]
pub struct SectionHeader {
    pub kind: SectionKind,
    pub size: u32,
}

type Expr = Vec<Instruction>;

#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub enum SectionKind {
    Custom,
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    DataCount,
    Code,
    Data,
}

#[derive(Debug, Error)]
#[error("invalid section ID: expected one of {markers}; got {0:#04X}", markers=SectionKind::markers_formatted())]
pub struct InvalidSectionIdError(pub u8);

impl From<u8> for InvalidSectionIdError {
    fn from(b: u8) -> Self {
        Self(b)
    }
}

// Valid marker bytes for [SectionKind].
#[expect(non_upper_case_globals)]
static SectionId_MARKERS: phf::OrderedMap<u8, SectionKind> = phf_ordered_map! {
            0u8 => SectionKind::Custom,
            1u8 => SectionKind::Type,
            2u8 => SectionKind::Import,
            3u8 => SectionKind::Function,
            4u8 => SectionKind::Table,
            5u8 => SectionKind::Memory,
            6u8 => SectionKind::Global,
            7u8 => SectionKind::Export,
            8u8 => SectionKind::Start,
            9u8 => SectionKind::Element,
            10u8 => SectionKind::Code,
            11u8 => SectionKind::Data,
            12u8 => SectionKind::DataCount,
};

impl FromMarkerByte for SectionKind {
    type Error = InvalidSectionIdError;

    fn markers() -> &'static phf::OrderedMap<u8, Self> {
        &SectionId_MARKERS
    }
}

/// The top-level error that may occur when attempting to decode bytes into
/// a [Module].
///
/// Encompasses all possible errors that may occur during decoding,
/// including section-specific errors.
#[derive(Debug, Error)]
pub enum DecodeModuleError {
    #[error(transparent)]
    ParsePreamble(#[from] ParsePreambleError),

    #[error("number of Code entries does not match number of Function entries")]
    CodeFuncEntriesLenMismatch { codes_len: usize, funcs_len: usize },

    #[error("number of Data segments does not match the Data Count section")]
    DataCountMismatch { datas_len: usize, data_count: u32 },

    #[error("out of order section: {current:?} cannot appear after {previous:?}")]
    SectionOutOfOrder {
        current: SectionKind,
        previous: SectionKind,
    },

    #[error("encountered duplicate section: {0:?}")]
    DuplicateSection(SectionKind),

    #[error("{section_kind:?} section size mismatch: declared {declared} bytes; got {got}")]
    SectionSizeMismatch {
        section_kind: SectionKind,
        declared: u32,
        got: u64,
    },

    #[error(
        "Data Count section does not match Datas length: declared {data_count}; got {datas_len}"
    )]
    DataCountDatasLenMismatch { datas_len: usize, data_count: u32 },

    #[error("failed decoding Data Count section")]
    DecodeDataCount(std::num::TryFromIntError),

    #[error("Data index was present in Code section, but Data Count section is missing")]
    DataIndexWithoutDataCount,

    // section-specific errors
    #[error(transparent)]
    DecodeSectionHeader(#[from] DecodeSectionHeaderError),

    #[error(transparent)]
    DecodeCustomSection(#[from] DecodeCustomSectionError),

    #[error(transparent)]
    DecodeTypeSection(#[from] DecodeTypeSectionError),

    #[error(transparent)]
    DecodeImportSection(#[from] DecodeImportSectionError),

    #[error(transparent)]
    DecodeFunctionSection(#[from] DecodeFunctionSectionError),

    #[error(transparent)]
    DecodeTableSection(#[from] DecodeTableSectionError),

    #[error(transparent)]
    DecodeMemorySection(#[from] DecodeMemorySectionError),

    #[error(transparent)]
    DecodeGlobalSection(#[from] DecodeGlobalSectionError),

    #[error(transparent)]
    DecodeExportSection(#[from] DecodeExportSectionError),

    #[error(transparent)]
    DecodeStartSection(#[from] DecodeStartSectionError),

    #[error(transparent)]
    DecodeElementSection(#[from] DecodeElementSectionError),

    #[error(transparent)]
    DecodeDatacountSection(#[from] DecodeDataCountSectionError),

    #[error(transparent)]
    DecodeCodeSection(#[from] DecodeCodeSectionError),

    #[error(transparent)]
    DecodeDataSection(#[from] DecodeDataSectionError),
}

/// Decode `input` into a WebAssembly [Module].
pub fn decode(mut input: impl Read) -> Result<Module, DecodeModuleError> {
    parse_preamble(&mut input)?;

    let mut module = Module {
        ..Default::default()
    };

    // track this to enforce that if the function section declares one or more
    // functions, then the code section is required (per section 5.5.16)
    let mut encountered_code_section = false;

    // track this to enforce that a data count section must be present if a
    // data index appears in one of the instructions of the Code section
    // (per section 5.5.16)
    let mut encountered_data_idx_in_code_section = false;

    while let Some(section_header) = decode_section_header(&mut input)? {
        let mut section_reader = &mut input.by_ref().take(section_header.size.into());

        module.validate_section_kind_expected(&section_header)?;

        let section_kind = section_header.kind;
        match section_kind {
            SectionKind::Custom => {
                module
                    .custom_sections
                    .push(decode_section_custom(&mut section_reader)?);
            }
            SectionKind::Type => module.types = decode_type_section(section_reader)?,
            SectionKind::Import => module.imports = decode_import_section(section_reader)?,
            SectionKind::Function => {
                for type_idx in decode_function_section(&mut section_reader)? {
                    module.funcs.push(Func {
                        r#type: type_idx,
                        // going to be filled later on by SectionKind::Code
                        locals: vec![],
                        body: vec![],
                    });
                }
            }
            SectionKind::Table => module.tables = decode_table_section(section_reader)?,
            SectionKind::Memory => module.mems = decode_memory_section(section_reader)?,
            SectionKind::Global => module.globals = decode_global_section(section_reader)?,
            SectionKind::Export => module.exports = decode_export_section(section_reader)?,
            SectionKind::Start => module.start = Some(decode_start_section(section_reader)?),
            SectionKind::Element => module.elems = decode_element_section(section_reader)?,
            SectionKind::DataCount => {
                module.data_count = Some(decode_datacount_section(section_reader)?)
            }
            SectionKind::Code => {
                encountered_code_section = true;

                let codes = decode_code_section(section_reader)?;
                if codes.len() != module.funcs.len() {
                    return Err(DecodeModuleError::CodeFuncEntriesLenMismatch {
                        codes_len: codes.len(),
                        funcs_len: module.funcs.len(),
                    });
                }

                for (i, code) in codes.into_iter().enumerate() {
                    for local in code.locals {
                        for _ in 0..local.count {
                            module.funcs[i].locals.push(local.t);
                        }
                    }

                    for instr in code.expr.iter() {
                        match instr {
                            Instruction::MemoryInit(_) | Instruction::DataDrop(_) => {
                                encountered_data_idx_in_code_section = true;
                                break;
                            }
                            _ => {}
                        }
                    }

                    module.funcs[i].body = code.expr;
                }
            }
            SectionKind::Data => {
                let datas = decode_data_section(section_reader)?;

                if let Some(data_count) = module.data_count
                    && datas.len() != usize::try_from(data_count).unwrap()
                {
                    return Err(DecodeModuleError::DataCountMismatch {
                        datas_len: datas.len(),
                        data_count,
                    });
                }
                module.datas = datas;
            }
        }

        if section_reader.limit() != 0 {
            return Err(DecodeModuleError::SectionSizeMismatch {
                section_kind,
                declared: section_header.size,
                got: u64::from(section_header.size) - section_reader.limit(),
            });
        }

        module.section_headers.push(section_header);
        if section_kind != SectionKind::Custom {
            // parsed_section_kinds facilitates enforcing the prescribed order of sections, but
            // Custom sections may appear in any order thus we don't want them in the set
            // to compare against
            module.parsed_section_kinds.push(section_kind);
        }
    }

    // Section 5.5.16: The lengths of vectors produced by the (possibly empty)
    // function and code section must match up
    if !module.funcs.is_empty() && !encountered_code_section {
        // if we encountered a Code section, it means we already checked that
        // its count matches the Function section count. (Function section comes
        // before Code section.)
        return Err(DecodeModuleError::CodeFuncEntriesLenMismatch {
            funcs_len: module.funcs.len(),
            codes_len: 0,
        });
    }

    // Section 5.5.16: Similarly, the optional data count must match the length
    // of the data segment vector
    if let Some(n) = module.data_count
        && usize::try_from(n).map_err(DecodeModuleError::DecodeDataCount)? != module.datas.len()
    {
        return Err(DecodeModuleError::DataCountDatasLenMismatch {
            datas_len: module.datas.len(),
            data_count: n,
        });
    }

    // Section 5.5.16: Furthermore, it must be present if any data index
    // occurs in the code section
    if encountered_data_idx_in_code_section && module.data_count.is_none() {
        return Err(DecodeModuleError::DataIndexWithoutDataCount);
    }

    Ok(module)
}

#[derive(Debug, Error)]
pub enum ParsePreambleError {
    #[error("failed decoding preamble")]
    Io(#[from] io::Error),

    #[error("unexpected preamble: expected {preamble:#X?}; got {0:#X?}", preamble=EXPECTED_PREAMBLE)]
    Unexpected([u8; 8]),
}

fn parse_preamble<R: Read + ?Sized>(reader: &mut R) -> Result<(), ParsePreambleError> {
    let mut preamble = [0u8; 8];
    reader.read_exact(&mut preamble)?;

    if preamble != EXPECTED_PREAMBLE {
        return Err(ParsePreambleError::Unexpected(preamble));
    }

    Ok(())
}

#[derive(Debug, Error)]
pub enum DecodeSectionHeaderError {
    #[error("failed reading section ID byte")]
    ReadSectionIdByte(#[from] io::Error),

    #[error("invalid section ID")]
    InvalidSectionId(#[from] InvalidSectionIdError),

    #[error("failed decoding section size")]
    DecodeSectionSize(#[from] DecodeU32Error),
}

fn decode_section_header<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Option<SectionHeader>, DecodeSectionHeaderError> {
    let id = match read_byte(reader) {
        Ok(id) => id,
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e.into()),
    };

    let kind = SectionKind::from_marker(id)?;
    let size = decode_u32(reader)?;

    Ok(Some(SectionHeader { kind, size }))
}

fn read_byte<R: Read + ?Sized>(reader: &mut R) -> Result<u8, io::Error> {
    let mut buf = [0u8];
    reader.read_exact(&mut buf)?;
    Ok(buf[0])
}
