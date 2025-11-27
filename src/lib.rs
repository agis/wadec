#![forbid(unsafe_code)]

pub mod index;
pub mod instr;
mod integer;

use crate::index::{FuncIdx, GlobalIdx, MemIdx, TableIdx, TypeIdx};
use crate::instr::{Instr, Parsed};
use anyhow::{Result, bail};
use integer::*;
use std::io;
use std::io::Read;
use thiserror::Error;

const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

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

    // the non-custom sections that were parsed
    // TODO: make this non-public
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
    fn validate_section_kind_expected(&self, next: &SectionHeader) -> Result<()> {
        let prev_kind = self.parsed_section_kinds.last();
        let next_kind = next.kind;

        // Every section is valid if it's the first one we're encountering.
        // Custom sections may appear anywhere in the module and multiple times
        if prev_kind.is_none() || next_kind == SectionKind::Custom {
            return Ok(());
        }

        if next_kind <= *prev_kind.unwrap() {
            bail!("unexpected {:?} section", next_kind);
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

/// Custom sections have the id 0. They are intended to be used for debugging information or
/// third-party extensions, and are ignored by the WebAssembly semantics. Their contents consist of
/// a name further identifying the custom section, followed by an uninterpreted sequence of bytes
/// for custom use.
///
/// <https://www.w3.org/TR/wasm-core-2/#custom-section>
/// <https://www.w3.org/TR/wasm-core-2/#binary-customsec>
#[derive(Debug, PartialEq)]
pub struct CustomSection {
    pub name: String,
    pub contents: Vec<u8>,
}

/// Function types classify the signature of functions, mapping a vector of parameters to a vector
/// of results. They are also used to classify the inputs and outputs of instructions.
///
/// <https://www.w3.org/TR/wasm-core-2/#function-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-functype>
#[derive(Debug, PartialEq)]
pub struct FuncType {
    pub parameters: Vec<ValType>,
    pub results: Vec<ValType>,
}

#[derive(Debug, Error)]
pub enum DecodeFuncTypeError {
    #[error(transparent)]
    ReadMarkerByte(#[from] io::Error),

    #[error("unexpected FuncType marker byte: expected 0x{expected:02X}; got 0x{0:02X}", expected = FuncType::MARKER_BYTE)]
    InvalidMarkerByte(u8),

    #[error("failed decoding Parameters")]
    DecodeParameterTypes(DecodeResultTypeError),

    #[error("failed decoding Results")]
    DecodeResultTypes(DecodeResultTypeError),
}

impl FuncType {
    const MARKER_BYTE: u8 = 0x60;

    pub fn read<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeFuncTypeError> {
        let b = read_byte(reader)?;
        if b != Self::MARKER_BYTE {
            return Err(DecodeFuncTypeError::InvalidMarkerByte(b));
        }

        let parameters =
            decode_result_type(reader).map_err(DecodeFuncTypeError::DecodeParameterTypes)?;
        let results = decode_result_type(reader).map_err(DecodeFuncTypeError::DecodeResultTypes)?;

        Ok(FuncType {
            parameters,
            results,
        })
    }
}

type Expr = Vec<Instr>;

/// The funcs component of a module defines a vector of functions.
///
/// Functions are referenced through function indices, starting with the
/// smallest index not referencing a function import.
///
/// <https://www.w3.org/TR/wasm-core-2/#functions>
/// <https://www.w3.org/TR/wasm-core-2/#code-section>
#[derive(Debug, PartialEq)]
pub struct Func {
    /// The type of a function declares its signature by reference to a type defined
    /// in the module. The parameters of the function are referenced through 0-based local indices
    /// in the function's body; they are mutable.
    pub r#type: TypeIdx,

    /// The locals declare a vector of mutable local variables and their types.
    /// These variables are referenced through local indices in the function's
    /// body. The index of the first local is the smallest index not referencing a
    /// parameter.
    pub locals: Vec<ValType>,

    /// The body is an instruction sequence that upon termination must produce an
    /// stack matching the function type's result type.
    pub body: Expr,
}

/// Table types classify tables over elements of reference type within a size range. Like
/// memories, tables are constrained by limits for their minimum and optionally maximum
/// size. The limits are given in numbers of entries.
///
/// <https://www.w3.org/TR/wasm-core-2/#table-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-tabletype>
#[derive(Debug, PartialEq)]
pub struct TableType {
    pub reftype: RefType,
    pub limits: Limits,
}

#[derive(Debug, Error)]
pub enum DecodeTableError {
    #[error(transparent)]
    DecodeRefType(#[from] DecodeRefTypeError),

    #[error(transparent)]
    DecodeLimits(#[from] ParseLimitsError),
}

impl TableType {
    pub fn read<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeTableError> {
        let reftype = RefType::read(reader)?;
        let limits = parse_limits(reader)?;
        Ok(TableType { reftype, limits })
    }
}

/// Memory types classify linear memories and their size range. The limits constrain the
/// minimum and optionally the maximum size of a memory. The limits are given in units of
/// page size.
///
/// <https://www.w3.org/TR/wasm-core-2/#memory-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-memtype>
#[derive(Debug, PartialEq)]
pub struct MemType {
    pub limits: Limits,
}

/// Limits classify the size range of resizeable storage associated with memory types and
/// table types. If no maximum is given, the respective storage can grow to any size.
///
/// <https://www.w3.org/TR/wasm-core-2/#limits>
/// <https://www.w3.org/TR/wasm-core-2/#binary-limits>
#[derive(Debug, PartialEq)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

/// The globals component of a module defines a vector of global variables (or globals for
/// short): Each global stores a single value of the given global type. Its type also
/// specifies whether a global is immutable or mutable. Moreover, each global is initialized
/// with an init value given by a constant initializer expression. Globals are referenced
/// through global indices, starting with the smallest index not referencing a global import.
///
/// <https://www.w3.org/TR/wasm-core-2/#globals>
/// <https://www.w3.org/TR/wasm-core-2/#global-section>
#[derive(Debug, PartialEq)]
pub struct Global {
    pub r#type: GlobalType,
    pub init: Expr,
}

/// Global types classify global variables, which hold a value and can either be mutable or
/// immutable.
///
/// <https://www.w3.org/TR/wasm-core-2/#global-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-globaltype>
#[derive(Debug, PartialEq)]
pub struct GlobalType(pub Mut, pub ValType);

#[derive(Debug, Error)]
pub enum DecodeGlobalTypeError {
    #[error("failed decoding Value type")]
    DecodeValueType(#[from] DecodeValTypeError),

    #[error("failed decoding Mutability")]
    DecodeMutability(std::io::Error),

    #[error(transparent)]
    InvalidMutability(#[from] InvalidMutabilityByteError),
}

impl GlobalType {
    fn read<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeGlobalTypeError> {
        let valtype = ValType::read(reader)?;
        let r#mut: Mut = read_byte(reader)
            .map_err(DecodeGlobalTypeError::DecodeMutability)?
            .try_into()?;
        Ok(GlobalType(r#mut, valtype))
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Mut {
    Const,
    Var,
}

#[derive(Debug, Error)]
#[error("invalid Mutability byte: expected 0x00 (const) or 0x01 (var); got 0x{0:02X}")]
pub struct InvalidMutabilityByteError(u8);

impl TryFrom<u8> for Mut {
    type Error = InvalidMutabilityByteError;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        Ok(match v {
            0x00 => Mut::Const,
            0x01 => Mut::Var,
            n => return Err(InvalidMutabilityByteError(n)),
        })
    }
}

/// Value types classify the individual values that WebAssembly code can compute with and
/// the values that a variable accepts. They are either number types, vector types, or
/// reference types.
///
/// <https://www.w3.org/TR/wasm-core-2/#value-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-valtype>
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ValType {
    Num(NumType),
    Vec(VecType),
    Ref(RefType),
}

#[derive(Debug, Error)]
#[error(
    "invalid ValType marker byte: expected \
    0x7F (int32), 0x7E (int64), 0x7D (float32), 0x7C (float64), \
    0x7B (v128), 0x70 (funcref) or 0x6F (externref); \
    got 0x{0:02X}"
)]
pub struct InvalidValTypeMarkerError(u8);

impl TryFrom<u8> for ValType {
    type Error = InvalidValTypeMarkerError;

    fn try_from(b: u8) -> Result<Self, Self::Error> {
        Ok(match b {
            0x7F => ValType::Num(NumType::Int32),
            0x7E => ValType::Num(NumType::Int64),
            0x7D => ValType::Num(NumType::Float32),
            0x7C => ValType::Num(NumType::Float64),
            0x7B => ValType::Vec(VecType::V128),
            0x70 => ValType::Ref(RefType::Func),
            0x6F => ValType::Ref(RefType::Extern),
            n => return Err(InvalidValTypeMarkerError(n)),
        })
    }
}

#[derive(Debug, Error)]
pub enum DecodeValTypeError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    InvalidMarkerByte(#[from] InvalidValTypeMarkerError),
}

impl ValType {
    fn read<R: Read + ?Sized>(reader: &mut R) -> Result<ValType, DecodeValTypeError> {
        Ok(read_byte(reader)?.try_into()?)
    }
}

/// Number types classify numeric values.
///
/// The types i32 and i64 classify 32 and 64 bit integers, respectively. Integers are not
/// inherently signed or unsigned, their interpretation is determined by individual operations.
///
/// The types f32 and f64 classify 32 and 64 bit floating-point data, respectively. They correspond to the respective binary
/// floating-point representations, also known as single and double precision, as defined by
/// the IEEE 754 standard (Section 3.3).
///
/// Number types are transparent, meaning that their bit patterns can be observed. Values of number
/// type can be stored in memories.
///
/// <https://www.w3.org/TR/wasm-core-2/#number-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-numtype>
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumType {
    Int32,
    Int64,
    Float32,
    Float64,
}

/// Vector types classify vectors of numeric values processed by vector instructions (also
/// known as SIMD instructions, single instruction multiple data).
///
/// The type v128 corresponds to a 128 bit vector of packed integer or floating-point data. The
/// packed data can be interpreted as signed or unsigned integers, single or double precision
/// floating-point values, or a single 128 bit type. The interpretation is determined by individual
/// operations.
///
/// Vector types, like number types are transparent, meaning that their bit patterns
/// can be observed. Values of vector type can be stored in memories.
///
/// <https://www.w3.org/TR/wasm-core-2/#vector-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-vectype>
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum VecType {
    V128,
}

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
#[error("invalid section ID: expected 0-12; got {0}")]
pub struct InvalidSectionIdError(u8);

impl TryFrom<u8> for SectionKind {
    type Error = InvalidSectionIdError;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        Ok(match v {
            0 => SectionKind::Custom,
            1 => SectionKind::Type,
            2 => SectionKind::Import,
            3 => SectionKind::Function,
            4 => SectionKind::Table,
            5 => SectionKind::Memory,
            6 => SectionKind::Global,
            7 => SectionKind::Export,
            8 => SectionKind::Start,
            9 => SectionKind::Element,
            10 => SectionKind::Code,
            11 => SectionKind::Data,
            12 => SectionKind::DataCount,
            n => return Err(InvalidSectionIdError(n)),
        })
    }
}

/// Reference types classify first-class references to objects in the runtime store.
///
/// The type funcref denotes the infinite union of all references to functions, regardless of
/// their function types.
///
/// The type externref denotes the infinite union of all references to
/// objects owned by the embedder and that can be passed into WebAssembly under this type.
///
/// Reference types are opaque, meaning that neither their size nor their bit pattern can be
/// observed. Values of reference type can be stored in tables.
///
/// <https://www.w3.org/TR/wasm-core-2/#reference-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-reftype>
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum RefType {
    Func,
    Extern,
}

#[derive(Debug, Error)]
pub enum DecodeRefTypeError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    InvalidMarkerByte(#[from] InvalidRefTypeMarkerError),
}

impl RefType {
    fn read<R: Read + ?Sized>(r: &mut R) -> Result<Self, DecodeRefTypeError> {
        Ok(read_byte(r)?.try_into()?)
    }
}

#[derive(Debug, Error)]
#[error("invalid RefType marker byte: expected 0x70 (funcref) or 0x6F (externref); got 0x{0:02X}")]
pub struct InvalidRefTypeMarkerError(u8);

impl TryFrom<u8> for RefType {
    type Error = InvalidRefTypeMarkerError;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        Ok(match v {
            0x70 => RefType::Func,
            0x6F => RefType::Extern,
            n => return Err(InvalidRefTypeMarkerError(n)),
        })
    }
}

pub fn decode(mut input: impl Read) -> Result<Module> {
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
                    bail!("code entries len do not match with funcs entries len");
                }

                for (i, code) in codes.into_iter().enumerate() {
                    for local in code.locals {
                        for _ in 0..local.count {
                            module.funcs[i].locals.push(local.t);
                        }
                    }

                    for instr in code.expr.iter() {
                        match instr {
                            Instr::MemoryInit(_) | Instr::DataDrop(_) => {
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
                    && datas.len() != data_count.try_into()?
                {
                    bail!(
                        "number of data segments ({}) do not match the data count section ({})",
                        data_count,
                        datas.len()
                    );
                }

                module.datas = datas;
            }
        }

        if section_reader.limit() != 0 {
            bail!(
                "section {:?} size mismatch: declared {} bytes, got {}",
                section_kind,
                section_header.size,
                u64::from(section_header.size) - section_reader.limit(),
            );
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
        bail!("function section has non-zero count but code section was absent")
    }

    // Section 5.5.16: Similarly, the optional data count must match the length
    // of the data segment vector
    if let Some(n) = module.data_count
        && usize::try_from(n)? != module.datas.len()
    {
        bail!("data count ({n}) was present but data segment did not match it",)
    }

    // Section 5.5.16: Furthermore, it must be present if any data index
    // occurs in the code section
    if encountered_data_idx_in_code_section && module.data_count.is_none() {
        bail!("data count section required because of data index in code section")
    }

    Ok(module)
}

#[derive(Debug, Error)]
pub enum DecodePreambleError {
    #[error("failed decoding preamble")]
    Io(#[from] io::Error),

    #[error(
        "unexpected preamble: expected [0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00]; got {0:#X?}"
    )]
    Unexpected([u8; 8]),
}

fn parse_preamble<R: Read + ?Sized>(reader: &mut R) -> Result<(), DecodePreambleError> {
    const MAGIC_NUMBER: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];

    let mut preamble = [0u8; 8];
    reader.read_exact(&mut preamble)?;

    if [MAGIC_NUMBER, VERSION].concat() != preamble {
        return Err(DecodePreambleError::Unexpected(preamble));
    }

    Ok(())
}

#[derive(Debug, Error)]
pub enum DecodeSectionHeaderError {
    #[error("failed reading section ID byte")]
    ReadSectionIdByte(#[from] io::Error),

    #[error(transparent)]
    InvalidSectionId(#[from] InvalidSectionIdError),

    #[error("failed decoding section size")]
    DecodeSectionSize(#[from] integer::DecodeError),
}

fn decode_section_header<R: Read + ?Sized>(reader: &mut R) -> Result<Option<SectionHeader>, DecodeSectionHeaderError> {
    let id = match read_byte(reader) {
        Ok(id) => id,
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e.into()),
    };

    let kind = SectionKind::try_from(id)?;
    let size = read_u32(reader)?;

    Ok(Some(SectionHeader { kind, size }))
}

#[derive(Debug, Error)]
pub enum DecodeCustomSectionError {
    #[error(transparent)]
    DecodeName(#[from] DecodeNameError),

    #[error("failed reading custom section contents")]
    Io(#[from] io::Error),
}

fn decode_section_custom<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<CustomSection, DecodeCustomSectionError> {
    let name = parse_name(reader)?;
    let mut contents = Vec::new();
    reader.read_to_end(&mut contents)?;

    Ok(CustomSection { name, contents })
}

#[derive(Debug, Error)]
pub enum DecodeTypeSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeFuncType(#[from] DecodeFuncTypeError),
}

fn decode_type_section<R: Read + ?Sized>(reader: &mut R) -> Result<Vec<FuncType>> {
    parse_vector(reader, FuncType::read)
}

#[derive(Debug, Error)]
pub enum DecodeFunctionSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error("failed decoding Type index")]
    DecodeTypeIdx(#[from] index::TypeIdxError),
}

fn decode_function_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<TypeIdx>, DecodeFunctionSectionError> {
    parse_vector(reader, TypeIdx::read)
}

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
#[derive(Debug, PartialEq)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDesc,
}

#[derive(Debug, PartialEq)]
pub enum ImportDesc {
    Type(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}

#[derive(Debug, Error)]
pub enum DecodeImportSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeImport(#[from] DecodeImportError),
}

fn decode_import_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Import>, DecodeImportSectionError> {
    parse_vector(reader, parse_import)
}

#[derive(Debug, Error)]
pub enum DecodeImportError {
    #[error("failed decoding module name")]
    DecodeModuleName(DecodeNameError),

    #[error("failed decoding entity name")]
    DecodeName(DecodeNameError),

    #[error("failed reading Import descriptor marker byte")]
    ReadDescriptorMarkerByte(io::Error),

    #[error(transparent)]
    DecodeTypeIdx(#[from] index::TypeIdxError),

    #[error(transparent)]
    DecodeTable(#[from] DecodeTableError),

    #[error(transparent)]
    DecodeMemType(#[from] DecodeMemoryTypeError),

    #[error(transparent)]
    DecodeGlobalType(#[from] DecodeGlobalTypeError),

    #[error(
        "invalid ImportDesc marker byte: expected 0x00 (type), 0x01 (table), 0x02 (mem) or 0x03 (global); got 0x{0:02X}"
    )]
    InvalidDescriptorMarkerByte(u8),
}

fn parse_import<R: Read + ?Sized>(reader: &mut R) -> Result<Import, DecodeImportError> {
    let module = parse_name(reader).map_err(DecodeImportError::DecodeModuleName)?;
    let name = parse_name(reader).map_err(DecodeImportError::DecodeName)?;

    // parse desc
    let mut desc_kind = [0u8];
    reader
        .read_exact(&mut desc_kind)
        .map_err(DecodeImportError::ReadDescriptorMarkerByte)?;
    let desc = match desc_kind[0] {
        0x00 => ImportDesc::Type(TypeIdx::read(reader)?),
        0x01 => ImportDesc::Table(TableType::read(reader)?),
        0x02 => ImportDesc::Mem(parse_memtype(reader)?),
        0x03 => ImportDesc::Global(GlobalType::read(reader)?),
        n => return Err(DecodeImportError::InvalidDescriptorMarkerByte(n)),
    };

    Ok(Import { module, name, desc })
}

/// The exports component of a module defines a set of exports that become accessible to the
/// host environment once the module has been instantiated. Each export is labeled by a
/// unique name. Exportable definitions are functions, tables, memories, and globals, which
/// are referenced through a respective descriptor.
///
/// <https://www.w3.org/TR/wasm-core-2/#exports>
/// <https://www.w3.org/TR/wasm-core-2/#export-section>
#[derive(Debug, PartialEq)]
pub struct Export {
    pub name: String,
    pub desc: ExportDesc,
}

#[derive(Debug, PartialEq)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug, Error)]
#[error(
    "invalid ExportDesc marker byte: expected 0x00 (func), 0x01 (table), 0x02 (mem) or 0x03 (global); got 0x{0:02X}"
)]
pub struct InvalidExportDescMarkerByte(u8);

impl ExportDesc {
    fn from(b: u8, idx: u32) -> Result<Self, InvalidExportDescMarkerByte> {
        Ok(match b {
            0x00 => ExportDesc::Func(FuncIdx(idx)),
            0x01 => ExportDesc::Table(TableIdx(idx)),
            0x02 => ExportDesc::Mem(MemIdx(idx)),
            0x03 => ExportDesc::Global(GlobalIdx(idx)),
            _ => return Err(InvalidExportDescMarkerByte(b)),
        })
    }
}

#[derive(Debug, Error)]
pub enum DecodeExportSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeExport(#[from] DecodeExportError),
}

// TODO: validate that names are unique?
fn decode_export_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Export>, DecodeExportSectionError> {
    parse_vector(reader, parse_export)
}

#[derive(Debug, Error)]
pub enum DecodeExportError {
    #[error(transparent)]
    DecodeName(#[from] DecodeNameError),

    #[error("failed reading Export descriptor marker byte")]
    ReadDescriptorMarkerByte(io::Error),

    #[error("failed reading ExportDesc index")]
    DecodeIndex(#[from] integer::DecodeError),

    #[error(transparent)]
    InvalidDescriptorMarkerByte(#[from] InvalidExportDescMarkerByte),
}

fn parse_export<R: Read + ?Sized>(reader: &mut R) -> Result<Export, DecodeExportError> {
    let name = parse_name(reader)?;

    let desc_kind = read_byte(reader).map_err(DecodeExportError::ReadDescriptorMarkerByte)?;
    let idx = read_u32(reader)?;
    let desc = ExportDesc::from(desc_kind, idx)?;

    Ok(Export { name, desc })
}

#[derive(Debug, Error)]
pub enum DecodeTableSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeTable(#[from] DecodeTableError),
}

fn decode_table_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<TableType>, DecodeTableSectionError> {
    parse_vector(reader, TableType::read)
}

#[derive(Debug, Error)]
pub enum DecodeMemorySectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeMemoryType(#[from] DecodeMemoryTypeError),
}

fn decode_memory_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<MemType>, DecodeMemorySectionError> {
    parse_vector(reader, parse_memtype)
}

#[derive(Debug, Error)]
#[error("failed decoding Memory type")]
pub struct DecodeMemoryTypeError(#[from] ParseLimitsError);

fn parse_memtype<R: Read + ?Sized>(reader: &mut R) -> Result<MemType, DecodeMemoryTypeError> {
    let limits = parse_limits(reader)?;
    Ok(MemType { limits })
}

#[derive(Debug, Error)]
pub enum DecodeGlobalSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeGlobal(#[from] DecodeGlobalError),
}

fn decode_global_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Global>, DecodeGlobalSectionError> {
    parse_vector(reader, parse_global)
}

#[derive(Debug, Error)]
pub enum DecodeGlobalError {
    #[error(transparent)]
    DecodeGlobalType(#[from] DecodeGlobalTypeError),

    #[error("failed decoding Init")]
    DecodeInit(#[from] ParseExpressionError),
}

fn parse_global<R: Read + ?Sized>(reader: &mut R) -> Result<Global, DecodeGlobalError> {
    Ok(Global {
        r#type: GlobalType::read(reader)?,
        init: parse_expr(reader)?,
    })
}

#[derive(Debug, PartialEq)]
pub struct Code {
    size: u32,
    locals: Vec<Local>,
    expr: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Local {
    count: u32,
    t: ValType,
}

#[derive(Debug, Error)]
pub enum DecodeCodeSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeCode(#[from] DecodeCodeError),
}

fn decode_code_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Code>, DecodeCodeSectionError> {
    parse_vector(reader, parse_code)
}

#[derive(Debug, Error)]
pub enum DecodeCodeError {
    #[error("failed decoding size of function code")]
    DecodeFunctionSize(integer::DecodeError),

    #[error("failed decoding locals vector length")]
    DecodeLocalsVectorLength(#[from] integer::DecodeError),

    #[error("failed decoding count of function locals")]
    DecodeLocalsCount(integer::DecodeError),

    #[error("too many locals: expected at most {max_locals}; got {actual_locals}")]
    LocalsCountOutOfBound { max_locals: u64, actual_locals: u64 },

    #[error("failed decoding local Value type")]
    DecodeLocalValType(#[from] DecodeValTypeError),

    #[error("failed decoding function body expression")]
    DecodeFunctionBody(#[from] ParseExpressionError),

    #[error("code entry size mismatch: declared {declared_bytes} bytes; leftover {leftover_bytes}")]
    EntrySizeMismatch {
        declared_bytes: u32,
        leftover_bytes: u64,
    },
}

fn parse_code<R: Read + ?Sized>(reader: &mut R) -> Result<Code, DecodeCodeError> {
    let size = read_u32(reader).map_err(DecodeCodeError::DecodeFunctionSize)?;

    let mut reader = reader.take(size.into());
    let mut expanded_locals: u64 = 0;
    let max_locals = u64::from(u32::MAX);

    let locals = parse_vector::<_, _, _, DecodeCodeError, _>(&mut reader, |r| {
        let count = read_u32(r)?;

        expanded_locals += u64::from(count);
        if expanded_locals > max_locals {
            return Err(DecodeCodeError::LocalsCountOutOfBound {
                max_locals,
                actual_locals: expanded_locals,
            });
        }

        Ok(Local {
            count,
            t: ValType::read(r)?,
        })
    })?;

    let expr = parse_expr(&mut reader)?;

    if reader.limit() != 0 {
        return Err(DecodeCodeError::EntrySizeMismatch {
            declared_bytes: size,
            leftover_bytes: reader.limit(),
        });
    }

    Ok(Code { size, locals, expr })
}

#[derive(Debug, Error)]
#[error("failed decoding Start section")]
pub struct DecodeStartSectionError(#[from] index::FuncIdxError);

fn decode_start_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<FuncIdx, DecodeStartSectionError> {
    Ok(FuncIdx::read(reader)?)
}

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
#[derive(Debug, PartialEq)]
pub struct Elem {
    pub r#type: RefType,
    pub init: Vec<Expr>,
    pub mode: ElemMode,
}

#[derive(Debug, PartialEq)]
pub enum ElemMode {
    Passive,
    Active { table: TableIdx, offset: Expr },
    Declarative,
}

#[derive(Debug, Error)]
pub enum DecodeElementSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeElement(#[from] DecodeElementError),
}

fn decode_element_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Elem>, DecodeElementSectionError> {
    parse_vector(reader, parse_elem)
}

#[derive(Debug, Error)]
pub enum DecodeElementError {
    #[error("failed decoding bitfield")]
    DecodeBitfield(integer::DecodeError),

    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error("invalid bitfield: expected value in range (0..7); got {0}")]
    InvalidBitfield(u32),

    #[error("failed decoding offset expression")]
    DecodeOffsetExpression(ParseExpressionError),

    #[error("failed decoding Element kind")]
    DecodeElementKind(#[from] DecodeElementKindError),

    #[error("failed decoding Element expression")]
    DecodeElementExpression(ParseExpressionError),

    #[error(transparent)]
    DecodeFuncIdx(#[from] index::FuncIdxError),

    #[error(transparent)]
    DecodeTableIdx(#[from] index::TableIdxError),

    #[error(transparent)]
    DecodeReferenceType(#[from] DecodeRefTypeError),

    #[error("failed decoding table.init expressions")]
    DecodeInit(ParseExpressionError),
}

fn parse_elem<R: Read + ?Sized>(reader: &mut R) -> Result<Elem, DecodeElementError> {
    let bitfield = read_u32(reader).map_err(DecodeElementError::DecodeBitfield)?;

    fn funcidx_into_reffunc(idxs: Vec<FuncIdx>) -> Vec<Expr> {
        idxs.into_iter()
            .map(|idx| vec![Instr::RefFunc(idx)])
            .collect()
    }

    let (r#type, init, mode) = match bitfield {
        0 => {
            let e = parse_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let y = parse_vector::<_, _, _, DecodeElementError, index::FuncIdxError>(
                reader,
                FuncIdx::read,
            )?;
            (
                RefType::Func,
                funcidx_into_reffunc(y),
                ElemMode::Active {
                    table: TableIdx(0),
                    offset: e,
                },
            )
        }
        1 => {
            let et = parse_elemkind(reader)?;
            let y = parse_vector::<_, _, _, DecodeElementError, index::FuncIdxError>(
                reader,
                FuncIdx::read,
            )?;
            (et, funcidx_into_reffunc(y), ElemMode::Passive)
        }
        2 => {
            let x = TableIdx::read(reader)?;
            let e = parse_expr(reader).map_err(DecodeElementError::DecodeElementExpression)?;
            let et = parse_elemkind(reader)?;
            let y = parse_vector::<_, _, _, DecodeElementError, index::FuncIdxError>(
                reader,
                FuncIdx::read,
            )?;
            (
                et,
                funcidx_into_reffunc(y),
                ElemMode::Active {
                    table: x,
                    offset: e,
                },
            )
        }
        3 => {
            let et = parse_elemkind(reader)?;
            let y = parse_vector::<_, _, _, DecodeElementError, index::FuncIdxError>(
                reader,
                FuncIdx::read,
            )?;
            (et, funcidx_into_reffunc(y), ElemMode::Declarative)
        }
        4 => {
            let e = parse_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let el = parse_vector::<_, _, _, DecodeElementError, _>(reader, |r| {
                parse_expr(r).map_err(DecodeElementError::DecodeInit)
            })?;
            (
                RefType::Func,
                el,
                ElemMode::Active {
                    table: TableIdx(0),
                    offset: e,
                },
            )
        }
        5 => {
            let et = RefType::read(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = parse_vector::<_, _, _, DecodeElementError, _>(reader, |r| {
                parse_expr(r).map_err(DecodeElementError::DecodeInit)
            })?;
            (et, el, ElemMode::Passive)
        }
        6 => {
            let x = TableIdx::read(reader)?;
            let e = parse_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let et = RefType::read(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = parse_vector::<_, _, _, DecodeElementError, _>(reader, |r| {
                parse_expr(r).map_err(DecodeElementError::DecodeInit)
            })?;

            (
                et,
                el,
                ElemMode::Active {
                    table: x,
                    offset: e,
                },
            )
        }
        7 => {
            let et = RefType::read(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = parse_vector::<_, _, _, DecodeElementError, _>(reader, |r| {
                parse_expr(r).map_err(DecodeElementError::DecodeInit)
            })?;

            (et, el, ElemMode::Declarative)
        }
        n => return Err(DecodeElementError::InvalidBitfield(n)),
    };

    Ok(Elem { r#type, init, mode })
}

#[derive(Debug, Error)]
pub enum DecodeElementKindError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("expected byte 0x00; got 0x{0:02X}")]
    InvalidElemKind(u8),
}

fn parse_elemkind<R: Read + ?Sized>(reader: &mut R) -> Result<RefType, DecodeElementKindError> {
    // we intentionally don't use RefType::read, since the spec uses 0x00
    // to mean `funcref`, but only in the legacy Element encodings
    let b = read_byte(reader)?;
    if b != 0x00 {
        return Err(DecodeElementKindError::InvalidElemKind(b));
    }
    Ok(RefType::Func)
}

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
#[derive(Debug, PartialEq)]
pub struct Data {
    pub init: Vec<u8>,
    pub mode: DataMode,
}

#[derive(Debug, PartialEq)]
pub enum DataMode {
    Passive,
    Active { memory: MemIdx, offset: Expr },
}

#[derive(Debug, Error)]
pub enum DecodeDataSectionError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeDataSegments(#[from] DecodeDataSegmentError),
}

fn decode_data_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Data>, DecodeDataSectionError> {
    parse_vector(reader, parse_data)
}

#[derive(Debug, Error)]
pub enum DecodeDataSegmentError {
    #[error("failed decoding bitfield")]
    DecodeBitfield(integer::DecodeError),

    #[error("invalid bitfield: expected 0 (passive), 1 or 2 (active); got {0}")]
    InvalidBitfield(u32),

    #[error("failed decoding offset expression")]
    DecodeOffsetExpr(ParseExpressionError),

    #[error("failed decoding init byte vector")]
    DecodeInitVector(#[from] DecodeByteVectorError),

    #[error("failed decoding Memory index")]
    DecodeMemIdx(integer::DecodeError),
}

fn parse_data<R: Read + ?Sized>(reader: &mut R) -> Result<Data, DecodeDataSegmentError> {
    let init: Vec<u8>;
    let mode: DataMode;

    (init, mode) = match read_u32(reader).map_err(DecodeDataSegmentError::DecodeBitfield)? {
        0 => {
            let e = parse_expr(reader).map_err(DecodeDataSegmentError::DecodeOffsetExpr)?;
            (
                parse_byte_vec(reader)?,
                DataMode::Active {
                    memory: MemIdx(0),
                    offset: e,
                },
            )
        }
        1 => (parse_byte_vec(reader)?, DataMode::Passive),
        2 => {
            let x = read_u32(reader).map_err(DecodeDataSegmentError::DecodeMemIdx)?;
            let e = parse_expr(reader).map_err(DecodeDataSegmentError::DecodeOffsetExpr)?;

            (
                parse_byte_vec(reader)?,
                DataMode::Active {
                    memory: MemIdx(x),
                    offset: e,
                },
            )
        }
        n => return Err(DecodeDataSegmentError::InvalidBitfield(n)),
    };

    Ok(Data { init, mode })
}

#[derive(Debug, Error)]
pub enum DecodeDataCountSectionError {
    #[error("failed decoding data segment count")]
    DecodeDataSegmentCount(#[from] integer::DecodeError),
}

fn decode_datacount_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<u32, DecodeDataCountSectionError> {
    Ok(read_u32(reader)?)
}

#[derive(Debug, Error)]
pub enum ParseExpressionError {
    #[error("failed parsing instruction")]
    ParseInstruction(#[from] anyhow::Error),

    #[error("unexpected Else delimiter")]
    UnexpectedElse,
}

fn parse_expr<R: Read + ?Sized>(reader: &mut R) -> Result<Expr, ParseExpressionError> {
    let mut body = Vec::new();

    loop {
        match Instr::parse(reader)? {
            Parsed::Instr(ins) => body.push(ins),
            Parsed::End => break,

            // `Else` is only expected to appear when parsing individual `Control` instructions
            Parsed::Else => return Err(ParseExpressionError::UnexpectedElse),
        }
    }

    Ok(body)
}

#[derive(Debug, Error)]
pub enum ParseLimitsError {
    #[error("failed determining presence of max limit")]
    DetermineMaxLimitPresence(io::Error),

    #[error("unexpected limits byte: expected 0x00 (false) or 0x01 (true); got 0x{0:02X}")]
    UnexpectedMaxLimitByte(u8),

    #[error("failed reading minimum limit")]
    ReadMinLimit(integer::DecodeError),

    #[error("failed reading maximum limit")]
    ReadMaxLimit(integer::DecodeError),

    #[error("failed reading limits byte")]
    ReadLimitsByte(integer::DecodeError),
}

fn parse_limits<R: Read + ?Sized>(reader: &mut R) -> Result<Limits, ParseLimitsError> {
    let has_max = match read_byte(reader).map_err(ParseLimitsError::DetermineMaxLimitPresence)? {
        0x00 => false,
        0x01 => true,
        n => return Err(ParseLimitsError::UnexpectedMaxLimitByte(n)),
    };

    let min = read_u32(reader).map_err(ParseLimitsError::ReadMinLimit)?;
    let mut max = None;

    if has_max {
        max = Some(read_u32(reader).map_err(ParseLimitsError::ReadMaxLimit)?);
    }

    Ok(Limits { min, max })
}

#[derive(Debug, Error)]
pub enum DecodeResultTypeError {
    #[error("failed decoding vector length")]
    DecodeVectorLength(#[from] integer::DecodeError),

    #[error(transparent)]
    DecodeValType(#[from] DecodeValTypeError),
}

pub fn decode_result_type<R: Read + ?Sized>(
    r: &mut R,
) -> Result<Vec<ValType>, DecodeResultTypeError> {
    parse_vector(r, ValType::read)
}

#[derive(Debug, Error)]
pub enum DecodeFloat32Error {
    #[error("failed reading 4 bytes for f32")]
    ReadPayload(#[from] io::Error),
}

fn parse_f32<R: Read + ?Sized>(r: &mut R) -> Result<f32, DecodeFloat32Error> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf)?;
    Ok(f32::from_le_bytes(buf))
}

#[derive(Debug, Error)]
pub enum DecodeFloat64Error {
    #[error("failed reading 8 bytes for f64")]
    ReadPayload(#[from] io::Error),
}

fn parse_f64<R: Read + ?Sized>(r: &mut R) -> Result<f64, DecodeFloat64Error> {
    let mut buf = [0u8; 8];
    r.read_exact(&mut buf)?;
    Ok(f64::from_le_bytes(buf))
}

fn parse_vector<R, F, T, E, E2>(reader: &mut R, mut parse_fn: F) -> Result<Vec<T>, E>
where
    R: Read + ?Sized,
    F: FnMut(&mut R) -> Result<T, E2>,
    E: From<E2> + From<integer::DecodeError>,
{
    let len = read_u32(reader)?;
    let mut items = Vec::with_capacity(len.try_into().unwrap());
    for _ in 0..len {
        items.push(parse_fn(reader)?);
    }

    Ok(items)
}

#[derive(Debug, Error)]
pub enum DecodeNameError {
    #[error(transparent)]
    DecodeByteVector(#[from] DecodeByteVectorError),

    #[error(transparent)]
    Utf8(#[from] std::string::FromUtf8Error),
}

fn parse_name<R: Read + ?Sized>(reader: &mut R) -> Result<String, DecodeNameError> {
    Ok(parse_byte_vec(reader)?.try_into()?)
}

#[derive(Debug, Error)]
pub enum DecodeByteVectorError {
    #[error("failed decoding vector length")]
    DecodeLength(#[from] integer::DecodeError),

    #[error("failed reading vector elements")]
    ReadElements(#[from] io::Error),
}

fn parse_byte_vec<R: Read + ?Sized>(reader: &mut R) -> Result<Vec<u8>, DecodeByteVectorError> {
    let len = read_u32(reader)?;
    let mut b = vec![0u8; len.try_into().unwrap()];
    reader.read_exact(&mut b)?;
    Ok(b)
}

fn read_byte<R: Read + ?Sized>(reader: &mut R) -> Result<u8, io::Error> {
    let mut buf = [0u8];
    reader.read_exact(&mut buf)?;
    Ok(buf[0])
}
