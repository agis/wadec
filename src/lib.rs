pub mod index;
pub mod instr;

use crate::index::*;
use crate::instr::{Instr, Parsed};
use anyhow::{bail, Result};
use std::io;
use std::io::Read;

const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

#[derive(Debug, PartialEq)]
pub struct Module {
    // only the non-custom sections that were parsed
    parsed_section_kinds: Vec<SectionKind>,

    pub version: [u8; 4],
    pub section_headers: Vec<SectionHeader>,
    data_count: Option<u32>,

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
pub struct GlobalType(Mut, ValType);

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
    fn read<R: io::Read>(reader: &mut R) -> Result<ValType> {
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
    fn read(r: &mut impl io::Read) -> Result<Self> {
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

    while let Some(section_header) = parse_section_header(&mut input)? {
        let mut section_reader = input.by_ref().take(section_header.size.into());
        let section_kind = section_header.kind;

        module.validate_section_kind_expected(&section_header)?;

        match section_kind {
            SectionKind::Custom => {
                module
                    .custom_sections
                    .push(parse_custom_section(&mut section_reader)?);
            }
            SectionKind::Type => module.types = parse_type_section(&mut section_reader)?,
            SectionKind::Import => module.imports = parse_import_section(&mut section_reader)?,
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
            SectionKind::Table => module.tables = parse_table_section(&mut section_reader)?,
            SectionKind::Memory => module.mems = parse_memory_section(&mut section_reader)?,
            SectionKind::Global => module.globals = parse_global_section(&mut section_reader)?,
            SectionKind::Export => module.exports = parse_export_section(&mut section_reader)?,
            SectionKind::Start => module.start = Some(parse_start_section(&mut section_reader)?),
            SectionKind::Element => module.elems = parse_element_section(&mut section_reader)?,
            SectionKind::DataCount => {
                module.data_count = Some(parse_datacount_section(&mut section_reader)?)
            }
            SectionKind::Code => {
                let codes = parse_code_section(&mut section_reader)?;
                if codes.len() != module.funcs.len() {
                    bail!("code entries len do not match with funcs entries len");
                }

                for (i, code) in codes.into_iter().enumerate() {
                    for local in code.locals {
                        for _ in 0..local.count {
                            module.funcs[i].locals.push(local.t);
                        }
                    }
                    module.funcs[i].body = code.expr;
                }
            }
            SectionKind::Data => {
                let datas = parse_data_section(&mut section_reader)?;

                if let Some(data_count) = module.data_count
                    && usize::try_from(data_count)? != datas.len()
                {
                    bail!(
                        "data segments do not match the data count section: {} - {}",
                        module.datas.len(),
                        data_count,
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

    Ok(module)
}

fn parse_preamble<R: io::Read>(reader: &mut R) -> Result<()> {
    const MAGIC_NUMBER: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];

    let mut preamble: [u8; 8] = [0u8; 8];
    reader.read_exact(&mut preamble)?;
    if [MAGIC_NUMBER, VERSION].concat() != preamble {
        bail!("unexpected preamble: {:#X?}", preamble);
    }

    Ok(())
}

fn parse_section_header<R: io::Read>(reader: &mut R) -> Result<Option<SectionHeader>> {
    let b = read_byte(reader);
    match b {
        Ok(_) => Ok::<(), anyhow::Error>(()),
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e.into()),
    }?;

    let kind = SectionKind::try_from(b.unwrap())?;
    let size = parse_u32(reader)?;

    Ok(Some(SectionHeader { kind, size }))
}

fn parse_custom_section<R: io::Read>(reader: &mut R) -> Result<CustomSection> {
    let name = parse_name(reader)?;
    let mut contents = Vec::new();
    reader.read_to_end(&mut contents)?;

    Ok(CustomSection { name, contents })
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-typesec
fn parse_type_section<R: io::Read>(reader: &mut R) -> Result<Vec<FuncType>> {
    parse_vec(reader, parse_functype)
}

fn parse_functype<R: io::Read>(reader: &mut R) -> Result<FuncType> {
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
fn parse_function_section<R: io::Read>(reader: &mut R) -> Result<Vec<TypeIdx>> {
    parse_vec(reader, |r| Ok(TypeIdx(parse_u32(r)?)))
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

fn parse_import_section<R: io::Read>(reader: &mut R) -> Result<Vec<Import>> {
    parse_vec(reader, parse_import)
}

fn parse_import<R: io::Read>(reader: &mut R) -> Result<Import> {
    let module = parse_name(reader)?;
    let name = parse_name(reader)?;

    // desc
    let mut desc_kind = [0u8];
    reader.read_exact(&mut desc_kind)?;
    let desc = match desc_kind[0] {
        0x00 => ImportDesc::Type(TypeIdx(parse_u32(reader)?)),
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
fn parse_export_section<R: io::Read>(reader: &mut R) -> Result<Vec<Export>> {
    parse_vec(reader, parse_export)
}

fn parse_export<R: io::Read>(reader: &mut R) -> Result<Export> {
    let name = parse_name(reader)?;

    let desc_kind = read_byte(reader)?;
    let idx = parse_u32(reader)?;
    let desc = ExportDesc::from(desc_kind, idx)?;

    Ok(Export { name, desc })
}

fn parse_table_section<R: io::Read>(reader: &mut R) -> Result<Vec<Table>> {
    parse_vec(reader, parse_table)
}

fn parse_table<R: io::Read>(reader: &mut R) -> Result<Table> {
    Ok(Table {
        reftype: RefType::read(reader)?,
        limits: parse_limits(reader)?,
    })
}

fn parse_memory_section<R: io::Read>(reader: &mut R) -> Result<Vec<Mem>> {
    parse_vec(reader, parse_memtype)
}

fn parse_memtype<R: io::Read>(reader: &mut R) -> Result<Mem> {
    Ok(Mem {
        limits: parse_limits(reader)?,
    })
}

fn parse_global_section<R: io::Read>(reader: &mut R) -> Result<Vec<Global>> {
    parse_vec(reader, |r| {
        Ok(Global {
            r#type: parse_globaltype(r)?,
            init: parse_expr(r)?,
        })
    })
}

fn parse_globaltype<R: io::Read>(reader: &mut R) -> Result<GlobalType> {
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

fn parse_code_section<R: io::Read>(reader: &mut R) -> Result<Vec<Code>> {
    parse_vec(reader, parse_code)
}

fn parse_code<R: io::Read>(reader: &mut R) -> Result<Code> {
    let size = parse_u32(reader)?;

    let mut reader = reader.take(size.into());

    let locals = parse_vec(&mut reader, |r| {
        Ok(Local {
            count: parse_u32(r)?,
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

fn parse_start_section<R: io::Read>(reader: &mut R) -> Result<FuncIdx> {
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

fn parse_element_section<R: io::Read>(reader: &mut R) -> Result<Vec<Elem>> {
    parse_vec(reader, parse_elem)
}

fn parse_elem<R: io::Read>(reader: &mut R) -> Result<Elem> {
    let bitfield = parse_u32(reader)?;

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

fn parse_elemkind<R: io::Read>(reader: &mut R) -> Result<RefType> {
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
    init: Vec<u8>,
    mode: DataMode,
}

#[derive(Debug, PartialEq)]
pub enum DataMode {
    Passive,
    Active { memory: MemIdx, offset: Expr },
}

fn parse_data_section<R: io::Read>(reader: &mut R) -> Result<Vec<Data>> {
    parse_vec(reader, parse_data)
}

fn parse_data<R: io::Read>(reader: &mut R) -> Result<Data> {
    let init: Vec<u8>;
    let mode: DataMode;

    (init, mode) = match parse_u32(reader)? {
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
            let x = parse_u32(reader)?;
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

fn parse_datacount_section<R: io::Read>(reader: &mut R) -> Result<u32> {
    parse_u32(reader)
}

fn parse_expr<R: io::Read>(reader: &mut R) -> Result<Expr> {
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

fn parse_limits<R: io::Read + ?Sized>(reader: &mut R) -> Result<Limits> {
    let has_max = match read_byte(reader)? {
        0x00 => false,
        0x01 => true,
        n => bail!("unexpected limits byte: {:x}", n),
    };

    let min = parse_u32(reader)?;
    let mut max = None;

    if has_max {
        max = Some(parse_u32(reader)?);
    }

    Ok(Limits { min, max })
}

fn parse_u32<R: io::Read + ?Sized>(reader: &mut R) -> Result<u32> {
    Ok(leb128::read::unsigned(reader)?.try_into()?)
}

fn parse_i32<R: io::Read + ?Sized>(reader: &mut R) -> Result<i32> {
    Ok(leb128::read::signed(reader)?.try_into()?)
}

fn parse_i64<R: io::Read + ?Sized>(reader: &mut R) -> Result<i64> {
    Ok(leb128::read::signed(reader)?)
}

fn parse_vec<R, T, F>(reader: &mut R, mut parse_item: F) -> Result<Vec<T>>
where
    R: io::Read + ?Sized,
    F: FnMut(&mut R) -> Result<T>,
{
    let len = parse_u32(reader)?;
    let mut items = Vec::with_capacity(len.try_into().unwrap());
    for _ in 0..len {
        items.push(parse_item(reader)?);
    }

    Ok(items)
}

fn parse_name<R: io::Read + ?Sized>(reader: &mut R) -> Result<String> {
    Ok(parse_byte_vec(reader)?.try_into()?)
}

fn read_byte<R: io::Read + ?Sized>(reader: &mut R) -> Result<u8, io::Error> {
    let mut buf = [0u8];
    reader.read_exact(&mut buf)?;
    Ok(buf[0])
}

fn parse_byte_vec<R: io::Read + ?Sized>(reader: &mut R) -> Result<Vec<u8>> {
    let len = parse_u32(reader)?;
    let mut b = vec![0u8; len.try_into().unwrap()];
    reader.read_exact(&mut b)?;
    Ok(b)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instr::{BlockType, Memarg};
    use pretty_assertions::assert_eq;
    use std::fs::File;

    #[test]
    fn parse_preamble() {
        let mut input: &[u8] = &[];
        assert!(decode(input).is_err());

        input = &[0xD3, 0xAD, 0xBE, 0xEF];
        assert!(decode(input).is_err());

        input = &[0xD3, 0xAD, 0xBE, 0xEF, 0x00, 0x00, 0x00, 0x00];
        assert!(decode(input).is_err());

        // just the preamble
        input = &[0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00];
        assert_eq!(decode(input).unwrap(), Module::default());
    }

    #[test]
    fn it_accepts_empty_module() {
        // (module)
        let f = File::open("./tests/fixtures/empty.wasm").unwrap();

        assert_eq!(decode(f).unwrap(), Module::default(),)
    }

    #[test]
    fn it_accepts_add_sample() {
        let f = File::open("./tests/fixtures/add.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Export,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 9,
            },
        ];

        let types = vec![FuncType {
            parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            results: vec![ValType::Num(NumType::Int32)],
        }];

        let funcs = vec![Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![
                Instr::LocalGet(LocalIdx(0)),
                Instr::LocalGet(LocalIdx(1)),
                Instr::I32Add,
            ],
        }];

        let exports = vec![Export {
            name: "add".to_owned(),
            desc: ExportDesc::Func(FuncIdx(0)),
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                exports,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_accepts_two_funcs_exporting_second() {
        // two funcs (i32,i32)->i32; exports func 1 as "add2"
        // Body: local.get 0, local.get 1, i32.add, end.
        let f = File::open("tests/fixtures/two_funcs_add2.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Export,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 8,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 17,
            },
        ];

        let types = vec![FuncType {
            parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            results: vec![ValType::Num(NumType::Int32)],
        }];

        let add_body = vec![
            Instr::LocalGet(LocalIdx(0)),
            Instr::LocalGet(LocalIdx(1)),
            Instr::I32Add,
        ];

        let funcs = vec![
            Func {
                r#type: TypeIdx(0),
                locals: Vec::new(),
                body: add_body.clone(),
            },
            Func {
                r#type: TypeIdx(0),
                locals: Vec::new(),
                body: add_body.clone(),
            },
        ];

        let exports = vec![Export {
            name: "add2".to_owned(),
            desc: ExportDesc::Func(FuncIdx(1)),
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                exports,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_accepts_imports_of_tables_memories_and_globals() {
        let f = File::open("tests/fixtures/imports_table_mem_global.wasm").unwrap();

        let section_headers = vec![SectionHeader {
            kind: SectionKind::Import,
            size: 63,
        }];

        let imports = vec![
            Import {
                module: "env".to_owned(),
                name: "table".to_owned(),
                desc: ImportDesc::Table(Table {
                    limits: Limits { min: 1, max: None },
                    reftype: RefType::Func,
                }),
            },
            Import {
                module: "env".to_owned(),
                name: "memory".to_owned(),
                desc: ImportDesc::Mem(Mem {
                    limits: Limits { min: 1, max: None },
                }),
            },
            Import {
                module: "env".to_owned(),
                name: "global_i".to_owned(),
                desc: ImportDesc::Global(GlobalType(Mut::Const, ValType::Num(NumType::Int32))),
            },
            Import {
                module: "env".to_owned(),
                name: "global_mut".to_owned(),
                desc: ImportDesc::Global(GlobalType(Mut::Var, ValType::Num(NumType::Int64))),
            },
        ];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds: vec![SectionKind::Import],
                section_headers,
                imports,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_accepts_module_without_exports() {
        // single func (i32,i32)->i32; no Export section
        // Body: local.get 0, local.get 1, i32.add
        let f = File::open("tests/fixtures/no_export.wasm").unwrap();

        let parsed_section_kinds =
            vec![SectionKind::Type, SectionKind::Function, SectionKind::Code];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 9,
            },
        ];

        let types = vec![FuncType {
            parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            results: vec![ValType::Num(NumType::Int32)],
        }];

        let funcs = vec![Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![
                Instr::LocalGet(LocalIdx(0)),
                Instr::LocalGet(LocalIdx(1)),
                Instr::I32Add,
            ],
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_decodes_start_section() {
        let f = File::open("tests/fixtures/start_section.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Start,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Start,
                size: 1,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 4,
            },
        ];

        let types = vec![FuncType {
            parameters: Vec::new(),
            results: Vec::new(),
        }];

        let funcs = vec![Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: Vec::new(),
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                start: Some(FuncIdx(0)),
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_decodes_control_instructions() {
        let f = File::open("tests/fixtures/control_instructions.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Table,
            SectionKind::Export,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 9,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Table,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 11,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 64,
            },
        ];

        let types = vec![
            FuncType {
                parameters: Vec::new(),
                results: Vec::new(),
            },
            FuncType {
                parameters: Vec::new(),
                results: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            },
        ];

        let funcs = vec![
            Func {
                r#type: TypeIdx(0),
                locals: Vec::new(),
                body: Vec::new(),
            },
            Func {
                r#type: TypeIdx(0),
                locals: Vec::new(),
                body: vec![
                    Instr::Nop,
                    Instr::Block(BlockType::Empty, vec![Instr::Unreachable]),
                    Instr::Block(
                        BlockType::Empty,
                        vec![Instr::Loop(BlockType::Empty, vec![Instr::Br(LabelIdx(1))])],
                    ),
                    Instr::Block(
                        BlockType::Empty,
                        vec![Instr::I32Const(0), Instr::BrIf(LabelIdx(0))],
                    ),
                    Instr::Block(
                        BlockType::Empty,
                        vec![
                            Instr::I32Const(0),
                            Instr::BrTable(vec![LabelIdx(0)], LabelIdx(0)),
                        ],
                    ),
                    Instr::Block(
                        BlockType::Empty,
                        vec![
                            Instr::I32Const(0),
                            Instr::If(
                                BlockType::Empty,
                                vec![Instr::Unreachable],
                                Some(vec![Instr::Nop]),
                            ),
                        ],
                    ),
                    Instr::Block(
                        BlockType::X(1),
                        vec![Instr::I32Const(42), Instr::I32Const(7)],
                    ),
                    Instr::Drop,
                    Instr::Drop,
                    Instr::Call(FuncIdx(0)),
                    Instr::I32Const(0),
                    Instr::CallIndirect(TableIdx(0), TypeIdx(0)),
                    Instr::Return,
                ],
            },
        ];

        let tables = vec![Table {
            limits: Limits { min: 1, max: None },
            reftype: RefType::Func,
        }];

        let exports = vec![Export {
            name: "control".to_owned(),
            desc: ExportDesc::Func(FuncIdx(1)),
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                tables,
                exports,
                ..Default::default()
            }
        )
    }
    #[test]
    fn it_decodes_element_section_all_alts() {
        let f = File::open("tests/fixtures/element_section_all_alts.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Table,
            SectionKind::Element,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Table,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Element,
                size: 67,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 19,
            },
        ];

        let types = vec![FuncType {
            parameters: vec![],
            results: vec![],
        }];

        let funcs = vec![
            Func {
                r#type: TypeIdx(0),
                locals: vec![],
                body: vec![],
            },
            Func {
                r#type: TypeIdx(0),
                locals: vec![],
                body: vec![],
            },
            Func {
                r#type: TypeIdx(0),
                locals: vec![],
                body: vec![],
            },
            Func {
                r#type: TypeIdx(0),
                locals: vec![],
                body: vec![],
            },
            Func {
                r#type: TypeIdx(0),
                locals: vec![],
                body: vec![],
            },
            Func {
                r#type: TypeIdx(0),
                locals: vec![],
                body: vec![],
            },
        ];

        let tables = vec![
            Table {
                limits: Limits { min: 12, max: None },
                reftype: RefType::Func,
            },
            Table {
                limits: Limits { min: 12, max: None },
                reftype: RefType::Func,
            },
        ];

        let elems = vec![
            Elem {
                r#type: RefType::Func,
                init: vec![
                    vec![Instr::RefFunc(FuncIdx(0))],
                    vec![Instr::RefFunc(FuncIdx(1))],
                ],
                mode: ElemMode::Active {
                    table: TableIdx(0),
                    offset: vec![Instr::I32Const(0)],
                },
            },
            Elem {
                r#type: RefType::Func,
                init: vec![
                    vec![Instr::RefFunc(FuncIdx(2))],
                    vec![Instr::RefFunc(FuncIdx(3))],
                ],
                mode: ElemMode::Passive,
            },
            Elem {
                r#type: RefType::Func,
                init: vec![vec![Instr::RefFunc(FuncIdx(4))]],
                mode: ElemMode::Active {
                    table: TableIdx(1),
                    offset: vec![Instr::I32Const(1)],
                },
            },
            Elem {
                r#type: RefType::Func,
                init: vec![vec![Instr::RefFunc(FuncIdx(5))]],
                mode: ElemMode::Declarative,
            },
            Elem {
                r#type: RefType::Func,
                init: vec![
                    vec![Instr::RefFunc(FuncIdx(0))],
                    vec![Instr::RefNull(RefType::Func)],
                ],
                mode: ElemMode::Active {
                    table: TableIdx(0),
                    offset: vec![Instr::I32Const(6)],
                },
            },
            Elem {
                r#type: RefType::Func,
                init: vec![
                    vec![Instr::RefFunc(FuncIdx(1))],
                    vec![Instr::RefNull(RefType::Func)],
                ],
                mode: ElemMode::Passive,
            },
            Elem {
                r#type: RefType::Func,
                init: vec![
                    vec![Instr::RefFunc(FuncIdx(2))],
                    vec![Instr::RefNull(RefType::Func)],
                ],
                mode: ElemMode::Active {
                    table: TableIdx(1),
                    offset: vec![Instr::I32Const(3)],
                },
            },
            Elem {
                r#type: RefType::Func,
                init: vec![
                    vec![Instr::RefNull(RefType::Func)],
                    vec![Instr::RefFunc(FuncIdx(3))],
                ],
                mode: ElemMode::Declarative,
            },
        ];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                tables,
                elems,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_decodes_reference_instructions() {
        let f = File::open("tests/fixtures/reference_instructions.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Table,
            SectionKind::Export,
            SectionKind::Element,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Table,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 8,
            },
            SectionHeader {
                kind: SectionKind::Element,
                size: 5,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 14,
            },
        ];

        let types = vec![FuncType {
            parameters: Vec::new(),
            results: Vec::new(),
        }];

        let funcs = vec![
            Func {
                r#type: TypeIdx(0),
                locals: Vec::new(),
                body: Vec::new(),
            },
            Func {
                r#type: TypeIdx(0),
                locals: Vec::new(),
                body: vec![
                    Instr::RefNull(RefType::Func),
                    Instr::RefIsNull,
                    Instr::Drop,
                    Instr::RefFunc(FuncIdx(0)),
                    Instr::Drop,
                ],
            },
        ];

        let tables = vec![Table {
            limits: Limits { min: 1, max: None },
            reftype: RefType::Func,
        }];

        let exports = vec![Export {
            name: "refs".to_owned(),
            desc: ExportDesc::Func(FuncIdx(1)),
        }];

        let elems = vec![Elem {
            r#type: RefType::Func,
            init: vec![vec![Instr::RefFunc(FuncIdx(0))]],
            mode: ElemMode::Declarative,
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                tables,
                elems,
                exports,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_decodes_variable_instructions() {
        let f = File::open("tests/fixtures/variable_instructions.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Global,
            SectionKind::Export,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 6,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Global,
                size: 6,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 20,
            },
        ];

        let types = vec![FuncType {
            parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            results: Vec::new(),
        }];

        let funcs = vec![Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![
                Instr::LocalGet(LocalIdx(0)),
                Instr::LocalSet(LocalIdx(1)),
                Instr::LocalGet(LocalIdx(1)),
                Instr::LocalTee(LocalIdx(0)),
                Instr::Drop,
                Instr::GlobalGet(GlobalIdx(0)),
                Instr::Drop,
                Instr::LocalGet(LocalIdx(1)),
                Instr::GlobalSet(GlobalIdx(0)),
            ],
        }];

        let globals = vec![Global {
            r#type: GlobalType(Mut::Var, ValType::Num(NumType::Int32)),
            init: vec![Instr::I32Const(0)],
        }];

        let exports = vec![Export {
            name: "var".to_owned(),
            desc: ExportDesc::Func(FuncIdx(0)),
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                globals,
                exports,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_decodes_parametric_instructions() {
        let f = File::open("tests/fixtures/parametric_instructions.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Export,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 8,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 9,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 24,
            },
        ];

        let types = vec![FuncType {
            parameters: vec![
                ValType::Num(NumType::Int32),
                ValType::Num(NumType::Int32),
                ValType::Num(NumType::Int32),
            ],
            results: vec![ValType::Num(NumType::Int32)],
        }];

        let funcs = vec![Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![
                Instr::LocalGet(LocalIdx(0)),
                Instr::LocalGet(LocalIdx(1)),
                Instr::LocalGet(LocalIdx(2)),
                Instr::Select(None),
                Instr::Drop,
                Instr::LocalGet(LocalIdx(0)),
                Instr::LocalGet(LocalIdx(1)),
                Instr::LocalGet(LocalIdx(2)),
                Instr::Select(Some(vec![ValType::Num(NumType::Int32)])),
                Instr::Drop,
                Instr::LocalGet(LocalIdx(0)),
            ],
        }];

        let exports = vec![Export {
            name: "param".to_owned(),
            desc: ExportDesc::Func(FuncIdx(0)),
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                exports,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_decodes_table_instructions() {
        let f = File::open("tests/fixtures/table_instructions.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Table,
            SectionKind::Export,
            SectionKind::Element,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 9,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Table,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 13,
            },
            SectionHeader {
                kind: SectionKind::Element,
                size: 9,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 66,
            },
        ];

        let types = vec![
            FuncType {
                parameters: Vec::new(),
                results: Vec::new(),
            },
            FuncType {
                parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
                results: Vec::new(),
            },
        ];

        let funcs = vec![
            Func {
                r#type: TypeIdx(0),
                locals: Vec::new(),
                body: Vec::new(),
            },
            Func {
                r#type: TypeIdx(1),
                locals: Vec::new(),
                body: vec![
                    Instr::LocalGet(LocalIdx(0)),
                    Instr::TableGet(TableIdx(0)),
                    Instr::Drop,
                    Instr::LocalGet(LocalIdx(0)),
                    Instr::RefNull(RefType::Func),
                    Instr::TableSet(TableIdx(0)),
                    Instr::I32Const(0),
                    Instr::I32Const(0),
                    Instr::I32Const(1),
                    Instr::TableInit(TableIdx(0), ElemIdx(1)),
                    Instr::ElemDrop(ElemIdx(1)),
                    Instr::I32Const(0),
                    Instr::RefNull(RefType::Func),
                    Instr::I32Const(1),
                    Instr::TableFill(TableIdx(0)),
                    Instr::TableSize(TableIdx(0)),
                    Instr::Drop,
                    Instr::RefNull(RefType::Func),
                    Instr::LocalGet(LocalIdx(1)),
                    Instr::TableGrow(TableIdx(0)),
                    Instr::Drop,
                    Instr::I32Const(0),
                    Instr::I32Const(0),
                    Instr::I32Const(1),
                    Instr::TableCopy(TableIdx(0), TableIdx(1)),
                    Instr::TableSize(TableIdx(0)),
                    Instr::Drop,
                ],
            },
        ];

        let tables = vec![
            Table {
                limits: Limits { min: 4, max: None },
                reftype: RefType::Func,
            },
            Table {
                limits: Limits { min: 4, max: None },
                reftype: RefType::Func,
            },
        ];

        let elems = vec![
            Elem {
                r#type: RefType::Func,
                init: vec![vec![Instr::RefFunc(FuncIdx(0))]],
                mode: ElemMode::Passive,
            },
            Elem {
                r#type: RefType::Func,
                init: vec![vec![Instr::RefFunc(FuncIdx(0))]],
                mode: ElemMode::Passive,
            },
        ];

        let exports = vec![Export {
            name: "table_ops".to_owned(),
            desc: ExportDesc::Func(FuncIdx(1)),
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                tables,
                elems,
                exports,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_decodes_memory_instructions() {
        let f = File::open("tests/fixtures/memory_instructions.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Memory,
            SectionKind::Export,
            SectionKind::DataCount,
            SectionKind::Code,
            SectionKind::Data,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Memory,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 20,
            },
            SectionHeader {
                kind: SectionKind::DataCount,
                size: 1,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 201,
            },
            SectionHeader {
                kind: SectionKind::Data,
                size: 46,
            },
        ];

        let types = vec![FuncType {
            parameters: Vec::new(),
            results: Vec::new(),
        }];

        let funcs = vec![Func {
            r#type: TypeIdx(0),
            locals: vec![
                ValType::Num(NumType::Float32),
                ValType::Num(NumType::Float64),
            ],
            body: vec![
                Instr::I32Const(0),
                Instr::I32Load(Memarg {
                    align: 2,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I64Load(Memarg {
                    align: 3,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::F32Load(Memarg {
                    align: 2,
                    offset: 0,
                }),
                Instr::LocalTee(LocalIdx(0)),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::F64Load(Memarg {
                    align: 3,
                    offset: 0,
                }),
                Instr::LocalTee(LocalIdx(1)),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I32Load8s(Memarg {
                    align: 0,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I32Load8u(Memarg {
                    align: 0,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I32Load16s(Memarg {
                    align: 1,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I32Load16u(Memarg {
                    align: 1,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I64Load8s(Memarg {
                    align: 0,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I64Load8u(Memarg {
                    align: 0,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I64Load16s(Memarg {
                    align: 1,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I64Load16u(Memarg {
                    align: 1,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I64Load32s(Memarg {
                    align: 2,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I64Load32u(Memarg {
                    align: 2,
                    offset: 0,
                }),
                Instr::Drop,
                Instr::I32Const(4),
                Instr::I32Const(1),
                Instr::I32Store(Memarg {
                    align: 2,
                    offset: 0,
                }),
                Instr::I32Const(8),
                Instr::I64Const(2),
                Instr::I64Store(Memarg {
                    align: 3,
                    offset: 0,
                }),
                Instr::I32Const(16),
                Instr::LocalGet(LocalIdx(0)),
                Instr::F32Store(Memarg {
                    align: 2,
                    offset: 0,
                }),
                Instr::I32Const(24),
                Instr::LocalGet(LocalIdx(1)),
                Instr::F64Store(Memarg {
                    align: 3,
                    offset: 0,
                }),
                Instr::I32Const(32),
                Instr::I32Const(5),
                Instr::I32Store8(Memarg {
                    align: 0,
                    offset: 0,
                }),
                Instr::I32Const(34),
                Instr::I32Const(6),
                Instr::I32Store16(Memarg {
                    align: 1,
                    offset: 0,
                }),
                Instr::I32Const(36),
                Instr::I64Const(7),
                Instr::I64Store8(Memarg {
                    align: 0,
                    offset: 0,
                }),
                Instr::I32Const(38),
                Instr::I64Const(8),
                Instr::I64Store16(Memarg {
                    align: 1,
                    offset: 0,
                }),
                Instr::I32Const(40),
                Instr::I64Const(9),
                Instr::I64Store32(Memarg {
                    align: 2,
                    offset: 0,
                }),
                Instr::MemorySize,
                Instr::Drop,
                Instr::I32Const(0),
                Instr::MemoryGrow,
                Instr::Drop,
                Instr::I32Const(0),
                Instr::I32Const(0),
                Instr::I32Const(4),
                Instr::MemoryInit(DataIdx(1)),
                Instr::DataDrop(DataIdx(1)),
                Instr::I32Const(8),
                Instr::I32Const(0),
                Instr::I32Const(4),
                Instr::MemoryCopy,
                Instr::I32Const(12),
                Instr::I32Const(255),
                Instr::I32Const(4),
                Instr::MemoryFill,
            ],
        }];

        let mems = vec![Mem {
            limits: Limits { min: 1, max: None },
        }];

        let exports = vec![
            Export {
                name: "mem".to_owned(),
                desc: ExportDesc::Mem(MemIdx(0)),
            },
            Export {
                name: "use-memory".to_owned(),
                desc: ExportDesc::Func(FuncIdx(0)),
            },
        ];

        let datas = vec![
            Data {
                init: vec![
                    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C,
                    0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
                    0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
                ],
                mode: DataMode::Active {
                    memory: MemIdx(0),
                    offset: vec![Instr::I32Const(0)],
                },
            },
            Data {
                init: vec![0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF],
                mode: DataMode::Passive,
            },
        ];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                mems,
                datas,
                exports,
                data_count: Some(2),
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_accepts_export_with_locals() {
        // single func with 2 i32 locals; exported "add_locals"
        // Body: local.get 0, local.get 1, i32.add, end.
        let f = File::open("tests/fixtures/with_locals_exported.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Function,
            SectionKind::Export,
            SectionKind::Code,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Type,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 14,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 11,
            },
        ];

        let types = vec![FuncType {
            parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            results: vec![ValType::Num(NumType::Int32)],
        }];

        let funcs = vec![Func {
            r#type: TypeIdx(0),
            locals: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            body: vec![
                Instr::LocalGet(LocalIdx(0)),
                Instr::LocalGet(LocalIdx(1)),
                Instr::I32Add,
            ],
        }];

        let exports = vec![Export {
            name: "add_locals".to_owned(),
            desc: ExportDesc::Func(FuncIdx(0)),
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                types,
                funcs,
                exports,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_accepts_foo() {
        let f = File::open("./tests/fixtures/foo.wasm").unwrap();

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Import,
            SectionKind::Function,
            SectionKind::Table,
            SectionKind::Memory,
            SectionKind::Global,
            SectionKind::Export,
            SectionKind::DataCount,
            SectionKind::Code,
            SectionKind::Data,
        ];

        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Custom,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Type,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Import,
                size: 11,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 2,
            },
            SectionHeader {
                kind: SectionKind::Table,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Memory,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Global,
                size: 6,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 23,
            },
            SectionHeader {
                kind: SectionKind::DataCount,
                size: 1,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 9,
            },
            SectionHeader {
                kind: SectionKind::Data,
                size: 7,
            },
        ];

        let custom_sections = vec![CustomSection {
            name: "note".to_owned(),
            contents: vec![0x68, 0x69],
        }];

        let types = vec![FuncType {
            parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
            results: vec![ValType::Num(NumType::Int32)],
        }];

        let imports = vec![Import {
            module: "env".to_owned(),
            name: "imp".to_owned(),
            desc: ImportDesc::Type(TypeIdx(0)),
        }];

        let tables = vec![Table {
            limits: Limits { min: 1, max: None },
            reftype: RefType::Func,
        }];

        let mems = vec![Mem {
            limits: Limits { min: 1, max: None },
        }];

        let globals = vec![Global {
            r#type: GlobalType(Mut::Const, ValType::Num(NumType::Int32)),
            init: vec![Instr::I32Const(42)],
        }];

        let exports = vec![
            Export {
                name: "add".to_owned(),
                desc: ExportDesc::Func(FuncIdx(1)),
            },
            Export {
                name: "mem".to_owned(),
                desc: ExportDesc::Mem(MemIdx(0)),
            },
            Export {
                name: "tab".to_owned(),
                desc: ExportDesc::Table(TableIdx(0)),
            },
            Export {
                name: "g".to_owned(),
                desc: ExportDesc::Global(GlobalIdx(0)),
            },
        ];

        let funcs = vec![Func {
            r#type: TypeIdx(0),
            locals: Vec::new(),
            body: vec![
                Instr::LocalGet(LocalIdx(0)),
                Instr::LocalGet(LocalIdx(1)),
                Instr::I32Add,
            ],
        }];

        let datas = vec![Data {
            init: vec![0x58],
            mode: DataMode::Active {
                memory: MemIdx(0),
                offset: vec![Instr::I32Const(0)],
            },
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                custom_sections,
                types,
                funcs,
                tables,
                mems,
                globals,
                datas,
                imports,
                exports,
                data_count: Some(1),
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_accepts_kitchensink() {
        let f = File::open("./tests/fixtures/kitchensink.wasm").unwrap();

        let custom_sections = vec![CustomSection {
            name: "note".to_owned(),
            contents: vec![0x68, 0x69], // hi
        }];

        let parsed_section_kinds = vec![
            SectionKind::Type,
            SectionKind::Import,
            SectionKind::Function,
            SectionKind::Table,
            SectionKind::Memory,
            SectionKind::Global,
            SectionKind::Export,
            SectionKind::Start,
            SectionKind::Element,
            SectionKind::Code,
            SectionKind::Data,
        ];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Custom,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Type,
                size: 10,
            },
            SectionHeader {
                kind: SectionKind::Import,
                size: 16,
            },
            SectionHeader {
                kind: SectionKind::Function,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Table,
                size: 4,
            },
            SectionHeader {
                kind: SectionKind::Memory,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Global,
                size: 6,
            },
            SectionHeader {
                kind: SectionKind::Export,
                size: 24,
            },
            SectionHeader {
                kind: SectionKind::Start,
                size: 1,
            },
            SectionHeader {
                kind: SectionKind::Element,
                size: 7,
            },
            SectionHeader {
                kind: SectionKind::Code,
                size: 14,
            },
            SectionHeader {
                kind: SectionKind::Data,
                size: 10,
            },
        ];

        let types = vec![
            FuncType {
                parameters: Vec::new(),
                results: Vec::new(),
            },
            FuncType {
                parameters: vec![ValType::Num(NumType::Int32), ValType::Num(NumType::Int32)],
                results: vec![ValType::Num(NumType::Int32)],
            },
        ];

        let funcs = vec![
            Func {
                r#type: TypeIdx(0),
                locals: Vec::new(),
                body: vec![Instr::Call(FuncIdx(0))],
            },
            Func {
                r#type: TypeIdx(1),
                locals: Vec::new(),
                body: vec![
                    Instr::LocalGet(LocalIdx(0)),
                    Instr::LocalGet(LocalIdx(1)),
                    Instr::I32Add,
                ],
            },
        ];

        let tables = vec![Table {
            limits: Limits { min: 1, max: None },
            reftype: RefType::Func,
        }];

        let mems = vec![Mem {
            limits: Limits { min: 1, max: None },
        }];

        let globals = vec![Global {
            r#type: GlobalType(Mut::Var, ValType::Num(NumType::Int32)),
            init: vec![Instr::I32Const(42)],
        }];

        let elems = vec![Elem {
            r#type: RefType::Func,
            init: vec![vec![Instr::RefFunc(FuncIdx(2))]],
            mode: ElemMode::Active {
                table: TableIdx(0),
                offset: vec![Instr::I32Const(0)],
            },
        }];

        let datas = vec![Data {
            init: vec![0, 17, 34, 51],
            mode: DataMode::Active {
                memory: MemIdx(0),
                offset: vec![Instr::I32Const(0)],
            },
        }];

        let imports = vec![Import {
            module: "env".to_owned(),
            name: "impstart".to_owned(),
            desc: ImportDesc::Type(TypeIdx(0)),
        }];

        let exports = vec![
            Export {
                name: "add".to_owned(),
                desc: ExportDesc::Func(FuncIdx(2)),
            },
            Export {
                name: "mem".to_owned(),
                desc: ExportDesc::Mem(MemIdx(0)),
            },
            Export {
                name: "tab".to_owned(),
                desc: ExportDesc::Table(TableIdx(0)),
            },
            Export {
                name: "g0".to_owned(),
                desc: ExportDesc::Global(GlobalIdx(0)),
            },
        ];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                custom_sections,
                types,
                funcs,
                tables,
                mems,
                globals,
                elems,
                datas,
                imports,
                exports,
                start: Some(FuncIdx(1)),
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_decodes_data_section_multiple_segments() {
        let f = File::open("tests/fixtures/data_section_multi_segment.wasm").unwrap();

        let parsed_section_kinds = vec![SectionKind::Memory, SectionKind::Data];
        let section_headers = vec![
            SectionHeader {
                kind: SectionKind::Memory,
                size: 3,
            },
            SectionHeader {
                kind: SectionKind::Data,
                size: 14,
            },
        ];

        let mems = vec![Mem {
            limits: Limits { min: 1, max: None },
        }];

        let datas = vec![
            Data {
                init: vec![0x41],
                mode: DataMode::Active {
                    memory: MemIdx(0),
                    offset: vec![Instr::I32Const(0)],
                },
            },
            Data {
                init: vec![0x42],
                mode: DataMode::Active {
                    memory: MemIdx(0),
                    offset: vec![Instr::I32Const(1)],
                },
            },
        ];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds,
                section_headers,
                mems,
                datas,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_respects_mem_limits() {
        let f = File::open("tests/fixtures/mem_limits_bug.wasm").unwrap();

        let section_headers = vec![SectionHeader {
            kind: SectionKind::Memory,
            size: 5,
        }];

        let mems = vec![Mem {
            limits: Limits {
                min: 1,
                max: Some(129),
            },
        }];

        assert_eq!(
            decode(f).unwrap(),
            Module {
                parsed_section_kinds: vec![SectionKind::Memory],
                section_headers,
                mems,
                ..Default::default()
            }
        )
    }

    #[test]
    fn it_fails_on_code_size_mismatch() {
        let f = File::open("tests/fixtures/code_section_size_underreported.wasm").unwrap();
        assert!(decode(f).is_err());

        let f = File::open("tests/fixtures/custom_section_size_overreported.wasm").unwrap();
        assert!(decode(f).is_err())
    }

    #[test]
    fn it_fails_on_code_entry_size_mismatch() {
        let f = File::open("tests/fixtures/code_entry_size_overreported.wasm").unwrap();
        assert!(decode(f).is_err());
    }

    #[test]
    fn it_enforces_section_ordering() {
        let f = File::open("tests/fixtures/invalid_section_order.wasm").unwrap();
        assert!(decode(f).is_err());
    }
}
