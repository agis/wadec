#![forbid(unsafe_code)]

pub mod index;
pub mod instr;
mod integer;

use crate::index::*;
use crate::instr::{Instr, Parsed};
use anyhow::{Result, bail};
use std::io;
use std::io::Read;
use integer::*;

const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

#[derive(Debug, PartialEq)]
pub struct Module {
    // only the non-custom sections that were parsed
    pub parsed_section_kinds: Vec<SectionKind>,

    pub version: [u8; 4],
    pub section_headers: Vec<SectionHeader>,
    pub data_count: Option<u32>,

    // sections
    pub custom_sections: Vec<CustomSection>,
    pub types: Vec<FuncType>,
    pub funcs: Vec<Func>,
    pub tables: Vec<Table>,
    pub mems: Vec<Mem>,
    pub globals: Vec<Global>,
    pub elems: Vec<Elem>,
    pub datas: Vec<Data>,
    pub start: Option<FuncIdx>,
    pub imports: Vec<Import>,
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

#[derive(PartialEq, Debug)]
pub struct SectionHeader {
    pub kind: SectionKind,
    pub size: u32,
}

#[derive(Debug, PartialEq)]
pub struct CustomSection {
    pub name: String,
    pub contents: Vec<u8>,
}

/// Function types classify the signature of functions, mapping a vector of parameters to a vector
/// of results. They are also used to classify the inputs and outputs of instructions.
///
/// https://webassembly.github.io/spec/core/syntax/types.html#syntax-functype
#[derive(Debug, PartialEq)]
pub struct FuncType {
    pub parameters: Vec<ValType>,
    pub results: Vec<ValType>,
}

type Expr = Vec<Instr>;

// https://webassembly.github.io/spec/core/syntax/modules.html#functions
#[derive(Debug, PartialEq)]
pub struct Func {
    pub r#type: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Table {
    pub limits: Limits,
    pub reftype: RefType,
}

#[derive(Debug, PartialEq)]
pub struct Mem {
    pub limits: Limits,
}

#[derive(Debug, PartialEq)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Debug, PartialEq)]
pub struct Global {
    pub r#type: GlobalType,
    pub init: Expr,
}

#[derive(Debug, PartialEq)]
pub struct GlobalType(pub Mut, pub ValType);

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Mut {
    Const,
    Var,
}

impl TryFrom<u8> for Mut {
    type Error = anyhow::Error;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        Ok(match v {
            0x00 => Mut::Const,
            0x01 => Mut::Var,
            n => bail!("malformed Mut: {:x}", n),
        })
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ValType {
    Num(NumType),
    Vec(VecType),
    Ref(RefType),
}

impl TryFrom<u8> for ValType {
    type Error = anyhow::Error;

    fn try_from(b: u8) -> Result<Self, Self::Error> {
        Ok(match b {
            0x7F => ValType::Num(NumType::Int32),
            0x7E => ValType::Num(NumType::Int64),
            0x7D => ValType::Num(NumType::Float32),
            0x7C => ValType::Num(NumType::Float64),
            0x7B => ValType::Vec(VecType::V128),
            0x70 => ValType::Ref(RefType::Func),
            0x6F => ValType::Ref(RefType::Extern),
            n => bail!("unexpected valtype: {:X}", n),
        })
    }
}

impl ValType {
    fn read<R: Read>(reader: &mut R) -> Result<ValType> {
        read_byte(reader)?.try_into()
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumType {
    Int32,
    Int64,
    Float32,
    Float64,
}

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

impl TryFrom<u8> for SectionKind {
    type Error = anyhow::Error;

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
            n => bail!("malformed section id {:x}", n),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum RefType {
    Func,
    Extern,
}

impl RefType {
    fn read<R: Read>(r: &mut R) -> Result<Self> {
        read_byte(r)?.try_into()
    }
}

impl TryFrom<u8> for RefType {
    type Error = anyhow::Error;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        Ok(match v {
            0x70 => RefType::Func,
            0x6F => RefType::Extern,
            n => bail!("malformed reftype: {:x}", n),
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

    while let Some(section_header) = parse_section_header(&mut input)? {
        let mut section_reader = &mut input.by_ref().take(section_header.size.into());

        module.validate_section_kind_expected(&section_header)?;

        let section_kind = section_header.kind;
        match section_kind {
            SectionKind::Custom => {
                module
                    .custom_sections
                    .push(parse_custom_section(&mut section_reader)?);
            }
            SectionKind::Type => module.types = parse_type_section(section_reader)?,
            SectionKind::Import => module.imports = parse_import_section(section_reader)?,
            SectionKind::Function => {
                for type_idx in parse_function_section(&mut section_reader)? {
                    module.funcs.push(Func {
                        r#type: type_idx,
                        // going to be filled later on by SectionKind::Code
                        locals: vec![],
                        body: vec![],
                    });
                }
            }
            SectionKind::Table => module.tables = parse_table_section(section_reader)?,
            SectionKind::Memory => module.mems = parse_memory_section(section_reader)?,
            SectionKind::Global => module.globals = parse_global_section(section_reader)?,
            SectionKind::Export => module.exports = parse_export_section(section_reader)?,
            SectionKind::Start => module.start = Some(parse_start_section(section_reader)?),
            SectionKind::Element => module.elems = parse_element_section(section_reader)?,
            SectionKind::DataCount => {
                module.data_count = Some(parse_datacount_section(section_reader)?)
            }
            SectionKind::Code => {
                encountered_code_section = true;

                let codes = parse_code_section(section_reader)?;
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
                let datas = parse_data_section(section_reader)?;

                if let Some(data_count) = module.data_count && datas.len() != data_count.try_into()? {
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
    if let Some(n) = module.data_count && usize::try_from(n)? != module.datas.len() {
        bail!("data count ({n}) was present but data segment did not match it",)
    }

    // Section 5.5.16: Furthermore, it must be present if any data index
    // occurs in the code section
    if encountered_data_idx_in_code_section && module.data_count.is_none() {
        bail!("data count section required because of data index in code section")
    }

    Ok(module)
}

fn parse_preamble<R: Read>(reader: &mut R) -> Result<()> {
    const MAGIC_NUMBER: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];

    let mut preamble: [u8; 8] = [0u8; 8];
    reader.read_exact(&mut preamble)?;
    if [MAGIC_NUMBER, VERSION].concat() != preamble {
        bail!("unexpected preamble: {:#X?}", preamble);
    }

    Ok(())
}

fn parse_section_header<R: Read>(reader: &mut R) -> Result<Option<SectionHeader>> {
    let b = read_byte(reader);
    match b {
        Ok(_) => Ok::<(), anyhow::Error>(()),
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e.into()),
    }?;

    let kind = SectionKind::try_from(b.unwrap())?;
    let size = read_u32(reader)?;

    Ok(Some(SectionHeader { kind, size }))
}

fn parse_custom_section<R: Read>(reader: &mut R) -> Result<CustomSection> {
    let name = parse_name(reader)?;
    let mut contents = Vec::new();
    reader.read_to_end(&mut contents)?;

    Ok(CustomSection { name, contents })
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-typesec
fn parse_type_section<R: Read>(reader: &mut R) -> Result<Vec<FuncType>> {
    parse_vec(reader, parse_functype)
}

fn parse_functype<R: Read>(reader: &mut R) -> Result<FuncType> {
    let b = read_byte(reader)?;
    if b != 0x60 {
        bail!("expected functype marker 0x60, got {:#X}", b);
    }

    let parameters = parse_vec(reader, ValType::read)?;
    let results = parse_vec(reader, ValType::read)?;

    Ok(FuncType {
        parameters,
        results,
    })
}

// https://webassembly.github.io/spec/core/binary/modules.html#function-section
fn parse_function_section<R: Read>(reader: &mut R) -> Result<Vec<TypeIdx>> {
    parse_vec(reader, TypeIdx::read)
}

#[derive(Debug, PartialEq)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDesc,
}

#[derive(Debug, PartialEq)]
pub enum ImportDesc {
    Type(TypeIdx),
    Table(Table),
    Mem(Mem),
    Global(GlobalType),
}

// https://webassembly.github.io/spec/core/binary/modules.html#import-section
fn parse_import_section<R: Read>(reader: &mut R) -> Result<Vec<Import>> {
    parse_vec(reader, parse_import)
}

fn parse_import<R: Read>(reader: &mut R) -> Result<Import> {
    let module = parse_name(reader)?;
    let name = parse_name(reader)?;

    // desc
    let mut desc_kind = [0u8];
    reader.read_exact(&mut desc_kind)?;
    let desc = match desc_kind[0] {
        0x00 => ImportDesc::Type(TypeIdx::read(reader)?),
        0x01 => ImportDesc::Table(parse_table(reader)?),
        0x02 => ImportDesc::Mem(parse_memtype(reader)?),
        0x03 => ImportDesc::Global(parse_globaltype(reader)?),
        n => bail!("unexpected import_desc: {n}"),
    };

    Ok(Import { module, name, desc })
}

// https://webassembly.github.io/spec/core/syntax/modules.html#exportsRT
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

impl ExportDesc {
    fn from(b: u8, idx: u32) -> Result<Self> {
        Ok(match b {
            0x00 => ExportDesc::Func(FuncIdx(idx)),
            0x01 => ExportDesc::Table(TableIdx(idx)),
            0x02 => ExportDesc::Mem(MemIdx(idx)),
            0x03 => ExportDesc::Global(GlobalIdx(idx)),
            _ => bail!("unexpected export_desc byte: {:X}", b),
        })
    }
}

// TODO: validate that names are unique?
fn parse_export_section<R: Read>(reader: &mut R) -> Result<Vec<Export>> {
    parse_vec(reader, parse_export)
}

fn parse_export<R: Read>(reader: &mut R) -> Result<Export> {
    let name = parse_name(reader)?;

    let desc_kind = read_byte(reader)?;
    let idx = read_u32(reader)?;
    let desc = ExportDesc::from(desc_kind, idx)?;

    Ok(Export { name, desc })
}

fn parse_table_section<R: Read>(reader: &mut R) -> Result<Vec<Table>> {
    parse_vec(reader, parse_table)
}

fn parse_table<R: Read>(reader: &mut R) -> Result<Table> {
    Ok(Table {
        reftype: RefType::read(reader)?,
        limits: parse_limits(reader)?,
    })
}

fn parse_memory_section<R: Read>(reader: &mut R) -> Result<Vec<Mem>> {
    parse_vec(reader, parse_memtype)
}

fn parse_memtype<R: Read>(reader: &mut R) -> Result<Mem> {
    Ok(Mem {
        limits: parse_limits(reader)?,
    })
}

fn parse_global_section<R: Read>(reader: &mut R) -> Result<Vec<Global>> {
    parse_vec(reader, |r| {
        Ok(Global {
            r#type: parse_globaltype(r)?,
            init: parse_expr(r)?,
        })
    })
}

fn parse_globaltype<R: Read>(reader: &mut R) -> Result<GlobalType> {
    let valtype = ValType::read(reader)?;
    let r#mut: Mut = read_byte(reader)?.try_into()?;
    Ok(GlobalType(r#mut, valtype))
}

#[derive(Debug, PartialEq)]
struct Code {
    size: u32,
    locals: Vec<Local>,
    expr: Expr,
}

#[derive(Debug, PartialEq)]
struct Local {
    count: u32,
    t: ValType,
}

fn parse_code_section<R: Read>(reader: &mut R) -> Result<Vec<Code>> {
    parse_vec(reader, parse_code)
}

fn parse_code<R: Read>(reader: &mut R) -> Result<Code> {
    let size = read_u32(reader)?;

    let mut reader = reader.take(size.into());
    let mut expanded_locals: u64 = 0;

    let locals = parse_vec(&mut reader, |r| {
        let count = read_u32(r)?;

        expanded_locals += count as u64;
        if expanded_locals > u32::MAX.into() {
            bail!("code locals out of bound: {}", expanded_locals);
        }

        Ok(Local {
            count,
            t: ValType::read(r)?,
        })
    })?;

    let expr = parse_expr(&mut reader)?;

    if reader.limit() != 0 {
        bail!(
            "code entry size mismatch: declared {} bytes, leftover {}",
            size,
            reader.limit(),
        );
    }

    Ok(Code { size, locals, expr })
}

fn parse_start_section<R: Read>(reader: &mut R) -> Result<FuncIdx> {
    FuncIdx::read(reader)
}

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

fn parse_element_section<R: Read>(reader: &mut R) -> Result<Vec<Elem>> {
    parse_vec(reader, parse_elem)
}

fn parse_elem<R: Read>(reader: &mut R) -> Result<Elem> {
    let bitfield = read_u32(reader)?;

    fn funcidx_into_reffunc(idxs: Vec<FuncIdx>) -> Vec<Expr> {
        idxs.into_iter()
            .map(|idx| vec![Instr::RefFunc(idx)])
            .collect()
    }

    let (r#type, init, mode) = match bitfield {
        0 => {
            let e = parse_expr(reader)?;
            let y = parse_vec(reader, FuncIdx::read)?;

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
            let y = parse_vec(reader, FuncIdx::read)?;

            (et, funcidx_into_reffunc(y), ElemMode::Passive)
        }
        2 => {
            let x = TableIdx::read(reader)?;
            let e = parse_expr(reader)?;
            let et = parse_elemkind(reader)?;
            let y = parse_vec(reader, FuncIdx::read)?;

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
            let y = parse_vec(reader, FuncIdx::read)?;

            (et, funcidx_into_reffunc(y), ElemMode::Declarative)
        }
        4 => {
            let e = parse_expr(reader)?;
            let el = parse_vec(reader, parse_expr)?;

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
            let et = RefType::read(reader)?;
            let el = parse_vec(reader, parse_expr)?;

            (et, el, ElemMode::Passive)
        }
        6 => {
            let x = TableIdx::read(reader)?;
            let e = parse_expr(reader)?;
            let et = RefType::read(reader)?;
            let el = parse_vec(reader, parse_expr)?;

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
            let et = RefType::read(reader)?;
            let el = parse_vec(reader, parse_expr)?;

            (et, el, ElemMode::Declarative)
        }
        n => bail!("unexpected elem bitfield: {:x}", n),
    };

    Ok(Elem { r#type, init, mode })
}

fn parse_elemkind<R: Read>(reader: &mut R) -> Result<RefType> {
    // we intentionally don't use RefType::read, since the spec uses 0x00
    // to mean `funcref`, but only in the legacy Element encodings
    let b = read_byte(reader)?;
    if b != 0x00 {
        bail!("expected byte `0x00` for elemkind; got {:x}", b);
    }
    Ok(RefType::Func)
}

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

fn parse_data_section<R: Read>(reader: &mut R) -> Result<Vec<Data>> {
    parse_vec(reader, parse_data)
}

fn parse_data<R: Read>(reader: &mut R) -> Result<Data> {
    let init: Vec<u8>;
    let mode: DataMode;

    (init, mode) = match read_u32(reader)? {
        0 => {
            let e = parse_expr(reader)?;
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
            let x = read_u32(reader)?;
            let e = parse_expr(reader)?;

            (
                parse_byte_vec(reader)?,
                DataMode::Active {
                    memory: MemIdx(x),
                    offset: e,
                },
            )
        }
        n => bail!("unexpected data bitfield: {n}"),
    };

    Ok(Data { init, mode })
}

fn parse_datacount_section<R: Read>(reader: &mut R) -> Result<u32> {
    Ok(read_u32(reader)?)
}

fn parse_expr<R: Read>(reader: &mut R) -> Result<Expr> {
    let mut body = Vec::new();

    loop {
        match Instr::parse(reader)? {
            Parsed::Instr(ins) => body.push(ins),
            Parsed::End => break,

            // `Else` is only expected to appear when parsing individual `Control` instructions
            Parsed::Else => bail!("unexpected `Else` delimiter"),
        }
    }

    Ok(body)
}

fn parse_limits<R: Read + ?Sized>(reader: &mut R) -> Result<Limits> {
    let has_max = match read_byte(reader)? {
        0x00 => false,
        0x01 => true,
        n => bail!("unexpected limits byte: {:x}", n),
    };

    let min = read_u32(reader)?;
    let mut max = None;

    if has_max {
        max = Some(read_u32(reader)?);
    }

    Ok(Limits { min, max })
}

fn parse_f32<R: Read + ?Sized>(r: &mut R) -> Result<f32> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf)?;
    Ok(f32::from_le_bytes(buf))
}

fn parse_f64<R: Read + ?Sized>(r: &mut R) -> Result<f64> {
    let mut buf = [0u8; 8];
    r.read_exact(&mut buf)?;
    Ok(f64::from_le_bytes(buf))
}

fn parse_vec<R, T, F>(reader: &mut R, mut parse_item: F) -> Result<Vec<T>>
where
    R: Read + ?Sized,
    F: FnMut(&mut R) -> Result<T>,
{
    let len = read_u32(reader)?;
    let mut items = Vec::with_capacity(len.try_into().unwrap());
    for _ in 0..len {
        items.push(parse_item(reader)?);
    }

    Ok(items)
}

fn parse_name<R: Read + ?Sized>(reader: &mut R) -> Result<String> {
    Ok(parse_byte_vec(reader)?.try_into()?)
}

fn read_byte<R: Read + ?Sized>(reader: &mut R) -> Result<u8, io::Error> {
    let mut buf = [0u8];
    reader.read_exact(&mut buf)?;
    Ok(buf[0])
}

fn parse_byte_vec<R: Read + ?Sized>(reader: &mut R) -> Result<Vec<u8>> {
    let len = read_u32(reader)?;
    let mut b = vec![0u8; len.try_into().unwrap()];
    reader.read_exact(&mut b)?;
    Ok(b)
}
