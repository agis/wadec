use anyhow::{anyhow, Result};
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
    // pub elems
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
            return Err(anyhow!("unexpected {:?} section", next_kind));
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
            n => return Err(anyhow!("malformed Mut: {:x}", n)),
        })
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ValType {
    Num(NumType),
    Vec(VecType),
    Ref(RefType),
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
            n => return Err(anyhow!("malformed section id {:x}", n)),
        })
    }
}

// https://webassembly.github.io/spec/core/syntax/modules.html#indices
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct TypeIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FuncIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct TableIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct MemIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct GlobalIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ElemIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DataIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LocalIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LabelIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum RefType {
    Func,
    Extern,
}

impl TryFrom<u8> for RefType {
    type Error = anyhow::Error;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        Ok(match v {
            0x70 => RefType::Func,
            0x6F => RefType::Extern,
            n => return Err(anyhow!("malformed reftype: {:x}", n)),
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
            SectionKind::DataCount => {
                module.data_count = Some(parse_datacount_section(&mut section_reader)?)
            }
            SectionKind::Code => {
                let codes = parse_code_section(&mut section_reader)?;
                if codes.len() != module.funcs.len() {
                    return Err(anyhow!(
                        "code entries len do not match with funcs entries len"
                    ));
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
                    return Err(anyhow!(
                        "data segments do not match the data count section: {} - {}",
                        module.datas.len(),
                        data_count,
                    ));
                }

                module.datas = datas;
            }
            n => panic!("not implemented section: `{:?}`", n),
        }

        if section_reader.limit() != 0 {
            return Err(anyhow!(
                "section {:?} size mismatch: declared {} bytes, got {}",
                section_kind,
                section_header.size,
                u64::from(section_header.size) - section_reader.limit(),
            ));
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

fn parse_preamble(mut input: impl io::Read) -> Result<()> {
    const MAGIC_NUMBER: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];

    let mut preamble: [u8; 8] = [0u8; 8];
    input.read_exact(&mut preamble)?;
    if [MAGIC_NUMBER, VERSION].concat() != preamble {
        return Err(anyhow!("unexpected preamble: {:#X?}", preamble));
    }

    Ok(())
}

fn parse_section_header(mut input: impl io::Read) -> Result<Option<SectionHeader>> {
    let mut buf = [0];
    match input.read_exact(&mut buf) {
        Ok(_) => Ok::<(), anyhow::Error>(()),
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e.into()),
    }?;

    let kind = SectionKind::try_from(buf[0])?;
    let size = parse_u32(&mut input)?;

    Ok(Some(SectionHeader { kind, size }))
}

fn parse_custom_section(mut input: impl io::Read) -> Result<CustomSection> {
    let name = parse_name(&mut input)?;
    let mut contents = Vec::new();
    input.read_to_end(&mut contents)?;

    Ok(CustomSection { name, contents })
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-typesec
fn parse_type_section(mut input: impl io::Read) -> Result<Vec<FuncType>> {
    parse_vec(&mut input, parse_functype)
}

fn parse_functype<R: io::Read>(input: &mut R) -> Result<FuncType> {
    let mut buf = [0u8];
    input.read_exact(&mut buf)?;
    if buf[0] != 0x60 {
        return Err(anyhow!(
            "expected functype marker 0x60, got 0x{:#x}",
            buf[0]
        ));
    }

    let parameters_count = parse_u32(&mut *input)?;
    let mut parameters = Vec::with_capacity(parameters_count.try_into().unwrap());
    for _ in 0..parameters_count {
        parameters.push(parse_valtype(input)?);
    }

    let results_count = parse_u32(&mut *input)?;
    let mut results = Vec::with_capacity(results_count.try_into().unwrap());
    for _ in 0..results_count {
        results.push(parse_valtype(input)?);
    }

    Ok(FuncType {
        parameters,
        results,
    })
}

// https://webassembly.github.io/spec/core/binary/modules.html#function-section
fn parse_function_section(mut input: impl io::Read) -> Result<Vec<TypeIdx>> {
    parse_vec(&mut input, |reader| Ok(TypeIdx(parse_u32(&mut *reader)?)))
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

fn parse_import_section(mut input: impl io::Read) -> Result<Vec<Import>> {
    parse_vec(&mut input, parse_import)
}

fn parse_import<R: io::Read>(input: &mut R) -> Result<Import> {
    let module = parse_name(&mut *input)?;
    let name = parse_name(&mut *input)?;

    // desc
    let mut desc_kind = [0u8];
    input.read_exact(&mut desc_kind)?;
    let desc = match desc_kind[0] {
        0x00 => ImportDesc::Type(TypeIdx(parse_u32(input)?)),
        0x01 => ImportDesc::Table(parse_table(input)?),
        0x02 => ImportDesc::Mem(parse_memtype(input)?),
        0x03 => ImportDesc::Global(parse_globaltype(input)?),
        n => {
            return Err(anyhow!("unexpected import_desc: {n}"));
        }
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

// TODO: validate that names are unique?
fn parse_export_section(mut input: impl io::Read) -> Result<Vec<Export>> {
    parse_vec(&mut input, parse_export)
}

fn parse_export<R: io::Read>(input: &mut R) -> Result<Export> {
    let name = parse_name(&mut *input)?;

    // desc
    let mut desc_kind = [0u8];
    input.read_exact(&mut desc_kind)?;
    let idx = parse_u32(&mut *input)?;
    let desc = match desc_kind[0] {
        0x00 => ExportDesc::Func(FuncIdx(idx)),
        0x01 => ExportDesc::Table(TableIdx(idx)),
        0x02 => ExportDesc::Mem(MemIdx(idx)),
        0x03 => ExportDesc::Global(GlobalIdx(idx)),
        // 0x01 => ExportDesc::Table(parse_table(&mut *input)?),
        // 0x02 => ExportDesc::Mem(MemIdx(idx)),
        // 0x03 => ExportDesc::Global(GlobalIdx(idx)),
        _ => {
            return Err(anyhow!("unexpected export_desc"));
        }
    };

    Ok(Export { name, desc })
}

fn parse_table_section(mut input: impl io::Read) -> Result<Vec<Table>> {
    parse_vec(&mut input, parse_table)
}

fn parse_table<R: io::Read>(input: &mut R) -> Result<Table> {
    let mut buf = [0u8];
    input.read_exact(&mut buf)?;
    let ref_type = RefType::try_from(buf[0])?;

    Ok(Table {
        reftype: ref_type,
        limits: parse_limits(&mut *input)?,
    })
}

fn parse_memory_section(mut input: impl io::Read) -> Result<Vec<Mem>> {
    parse_vec(&mut input, parse_memtype)
}

fn parse_memtype<R: io::Read>(input: &mut R) -> Result<Mem> {
    Ok(Mem {
        limits: parse_limits(input)?,
    })
}

fn parse_global_section(mut input: impl io::Read) -> Result<Vec<Global>> {
    parse_vec(&mut input, |reader| {
        Ok(Global {
            r#type: parse_globaltype(&mut *reader)?,
            init: parse_expr(&mut *reader)?,
        })
    })
}

fn parse_globaltype(mut input: impl io::Read) -> Result<GlobalType> {
    let valtype = parse_valtype(&mut input)?;
    let r#mut: Mut = read_byte(&mut input)?.try_into()?;
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

fn parse_code_section(mut input: impl io::Read) -> Result<Vec<Code>> {
    parse_vec(&mut input, parse_code)
}

fn parse_code<R: io::Read>(input: &mut R) -> Result<Code> {
    let size = parse_u32(&mut *input)?;

    let mut input = input.take(size.into());

    let locals = parse_vec(&mut input, |reader| {
        Ok(Local {
            count: parse_u32(&mut *reader)?,
            t: parse_valtype(&mut *reader)?,
        })
    })?;

    let expr = parse_expr(&mut input)?;

    if input.limit() != 0 {
        return Err(anyhow!(
            "code entry size mismatch: declared {} bytes, leftover {}",
            size,
            input.limit(),
        ));
    }

    Ok(Code { size, locals, expr })
}

fn parse_start_section(input: impl io::Read) -> Result<FuncIdx> {
    let idx = parse_u32(input)?;
    Ok(FuncIdx(idx))
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

fn parse_data_section(mut input: impl io::Read) -> Result<Vec<Data>> {
    parse_vec(&mut input, parse_data)
}

fn parse_data<R: io::Read>(input: &mut R) -> Result<Data> {
    let mut bitfield = [0u8];
    input.read_exact(&mut bitfield)?;

    let mut init = Vec::new();

    let mode = match bitfield[0] {
        0 => {
            let e = parse_expr(&mut *input)?;
            input.read_to_end(&mut init)?;
            DataMode::Active {
                memory: MemIdx(0),
                offset: e,
            }
        }
        1 => {
            input.read_to_end(&mut init)?;
            DataMode::Passive
        }
        2 => {
            let idx = parse_u32(&mut *input)?;
            let e = parse_expr(&mut *input)?;
            input.read_to_end(&mut init)?;
            DataMode::Active {
                memory: MemIdx(idx),
                offset: e,
            }
        }
        n => return Err(anyhow!("unexpected data bitfield: {n}")),
    };

    Ok(Data { init, mode })
}

fn parse_datacount_section(input: impl io::Read) -> Result<u32> {
    parse_u32(input)
}

// https://webassembly.github.io/spec/core/syntax/instructions.html#
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Instr {
    // --- Control instructions (5.4.1) ---
    Unreachable,  // unreachable
    Nop,          // nop
    Block,        // block <blocktype> ... end
    Loop,         // loop <blocktype> ... end
    If,           // if <blocktype> ... (else ...)? end
    Br,           // br
    BrIf,         // br_if
    BrTable,      // br_table
    Return,       // return
    Call,         // call
    CallIndirect, // call_indirect

    // --- Reference instructions (5.4.2) ---
    RefNull,   // ref.null
    RefIsNull, // ref.is_null
    RefFunc,   // ref.func

    // --- Parametric instructions (5.4.3) ---
    Drop,   // drop
    Select, // select  (typed form uses an immediate type vector)

    // --- Variable instructions (5.4.4) ---
    LocalGet(LocalIdx),   // local.get
    LocalSet(LocalIdx),   // local.set
    LocalTee(LocalIdx),   // local.tee
    GlobalGet(GlobalIdx), // global.get
    GlobalSet(GlobalIdx), // global.set

    // --- Table instructions (5.4.5) ---
    TableGet,  // table.get
    TableSet,  // table.set
    TableInit, // table.init
    ElemDrop,  // elem.drop
    TableCopy, // table.copy
    TableGrow, // table.grow
    TableSize, // table.size
    TableFill, // table.fill

    // --- Memory instructions (5.4.6) ---
    I32Load,    // i32.load
    I64Load,    // i64.load
    F32Load,    // f32.load
    F64Load,    // f64.load
    I32Load8S,  // i32.load8_s
    I32Load8U,  // i32.load8_u
    I32Load16S, // i32.load16_s
    I32Load16U, // i32.load16_u
    I64Load8S,  // i64.load8_s
    I64Load8U,  // i64.load8_u
    I64Load16S, // i64.load16_s
    I64Load16U, // i64.load16_u
    I64Load32S, // i64.load32_s
    I64Load32U, // i64.load32_u
    I32Store,   // i32.store
    I64Store,   // i64.store
    F32Store,   // f32.store
    F64Store,   // f64.store
    I32Store8,  // i32.store8
    I32Store16, // i32.store16
    I64Store8,  // i64.store8
    I64Store16, // i64.store16
    I64Store32, // i64.store32
    MemorySize, // memory.size
    MemoryGrow, // memory.grow
    MemoryInit, // memory.init
    DataDrop,   // data.drop
    MemoryCopy, // memory.copy
    MemoryFill, // memory.fill

    // --- Numeric constants (5.4.7) ---
    I32Const(i32), // i32.const
    I64Const(i64), // i64.const
    F32Const(f32), // f32.const
    F64Const(f64), // f64.const

    // --- I32 tests/relops (5.4.7) ---
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,

    // --- I64 tests/relops (5.4.7) ---
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,

    // --- F32 relops (5.4.7) ---
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,

    // --- F64 relops (5.4.7) ---
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,

    // --- I32 numeric ops (5.4.7) ---
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,

    // --- I64 numeric ops (5.4.7) ---
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,

    // --- F32 numeric ops (5.4.7) ---
    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,

    // --- F64 numeric ops (5.4.7) ---
    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,

    // --- Conversions & reinterpretations (5.4.7) ---
    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,

    // --- Sign-extension ops (5.4.7, “sign extension instructions”) ---
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,

    // --- Non-trapping float-to-int (saturating truncation, 5.4.7) ---
    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,

    // --- SIMD / Vector (5.4.8) — placeholder capturing 0xFD subopcode ---
    /// All SIMD instructions share the 0xFD prefix; this carries the subopcode (u32)
    Simd(u32),
}

fn parse_expr(mut input: impl io::Read) -> Result<Expr> {
    let mut instr = Vec::new();

    while let Some(i) = Instr::parse(&mut input)? {
        instr.push(i);
    }

    Ok(instr)
}

impl Instr {
    fn parse(mut input: impl io::Read) -> Result<Option<Self>> {
        let mut buf = [0u8];

        let _ = match input.read_exact(&mut buf) {
            Ok(_) => Ok::<(), anyhow::Error>(()),
            //    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
            Err(e) => return Err(e.into()),
        };

        Ok(match buf[0] {
            0x0B => None, // not an instruction, rather an opcode for `end`
            0x20 => Some(Instr::LocalGet(LocalIdx(parse_u32(&mut input)?))),
            0x41 => Some(Instr::I32Const(parse_i32(&mut input)?)),
            0x42 => Some(Instr::I64Const(parse_i64(&mut input)?)),
            0x6A => Some(Instr::I32Add),
            n => {
                return Err(anyhow!("unexpected instr: {:x}", n));
            }
        })
    }
}

fn parse_valtype(input: &mut impl io::Read) -> Result<ValType> {
    let mut buf = [0u8];
    input.read_exact(&mut buf)?;

    Ok(match buf[0] {
        0x7F => ValType::Num(NumType::Int32),
        0x7E => ValType::Num(NumType::Int64),
        0x7D => ValType::Num(NumType::Float32),
        0x7C => ValType::Num(NumType::Float64),
        0x7B => ValType::Vec(VecType::V128),
        0x70 => ValType::Ref(RefType::Func),
        0x6F => ValType::Ref(RefType::Extern),
        n => {
            return Err(anyhow!("unexpected valtype: {:x}", n));
        }
    })
}

fn parse_limits(mut input: impl io::Read) -> Result<Limits> {
    let mut buf = [0u8];
    input.read_exact(&mut buf)?;

    let has_max = match buf[0] {
        0x00 => false,
        0x01 => true,
        n => return Err(anyhow!("unexpected limits byte: {:x}", n)),
    };

    let min = parse_u32(&mut input)?;
    let mut max = None;

    if has_max {
        max = Some(parse_u32(input)?);
    }

    Ok(Limits { min, max })
}

fn parse_u32(mut input: impl io::Read) -> Result<u32> {
    Ok(leb128::read::unsigned(&mut input)?.try_into()?)
}

fn parse_i32(mut input: impl io::Read) -> Result<i32> {
    Ok(leb128::read::signed(&mut input)?.try_into()?)
}

fn parse_i64(mut input: impl io::Read) -> Result<i64> {
    Ok(leb128::read::signed(&mut input)?)
}

fn parse_vec<R, T, F>(input: &mut R, mut parse_item: F) -> Result<Vec<T>>
where
    R: io::Read + ?Sized,
    F: FnMut(&mut R) -> Result<T>,
{
    let len = parse_u32(&mut *input)?;
    let mut items = Vec::with_capacity(len.try_into().unwrap());
    for _ in 0..len {
        items.push(parse_item(&mut *input)?);
    }

    Ok(items)
}

fn parse_name(mut input: impl io::Read) -> Result<String> {
    let len = parse_u32(&mut input)?;
    let mut name = vec![0u8; len.try_into().unwrap()];
    input.read_exact(&mut name)?;
    Ok(name.try_into()?)
}

fn read_byte(mut input: impl io::Read) -> Result<u8> {
    let mut buf = [0u8];
    input.read_exact(&mut buf)?;
    Ok(buf[0])
}

#[cfg(test)]
mod tests {
    use super::*;
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
            init: vec![0x01, 0x58],
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

        assert_eq!(
            decode(f).unwrap(),
            Module {
                custom_sections,
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
