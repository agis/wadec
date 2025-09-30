use crate::*;

// https://webassembly.github.io/spec/core/syntax/instructions.html#
#[derive(Debug, Clone, PartialEq)]
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
    RefNull(RefType), // ref.null
    RefIsNull,        // ref.is_null
    RefFunc(FuncIdx), // ref.func

    // --- Parametric instructions (5.4.3) ---
    Drop,                         // drop
    Select(Option<Vec<ValType>>), // select

    // --- Variable instructions (5.4.4) ---
    LocalGet(LocalIdx),   // local.get
    LocalSet(LocalIdx),   // local.set
    LocalTee(LocalIdx),   // local.tee
    GlobalGet(GlobalIdx), // global.get
    GlobalSet(GlobalIdx), // global.set

    // --- Table instructions (5.4.5) ---
    TableGet(TableIdx),            // table.get
    TableSet(TableIdx),            // table.set
    TableInit(TableIdx, ElemIdx),  // table.init
    ElemDrop(ElemIdx),             // elem.drop
    TableCopy(TableIdx, TableIdx), // table.copy
    TableGrow(TableIdx),           // table.grow
    TableSize(TableIdx),           // table.size
    TableFill(TableIdx),           // table.fill

    // --- Memory instructions (5.4.6) ---
    I32Load(Memarg),     // i32.load
    I64Load(Memarg),     // i64.load
    F32Load(Memarg),     // f32.load
    F64Load(Memarg),     // f64.load
    I32Load8s(Memarg),   // i32.load8_s
    I32Load8u(Memarg),   // i32.load8_u
    I32Load16s(Memarg),  // i32.load16_s
    I32Load16u(Memarg),  // i32.load16_u
    I64Load8s(Memarg),   // i64.load8_s
    I64Load8u(Memarg),   // i64.load8_u
    I64Load16s(Memarg),  // i64.load16_s
    I64Load16u(Memarg),  // i64.load16_u
    I64Load32s(Memarg),  // i64.load32_s
    I64Load32u(Memarg),  // i64.load32_u
    I32Store(Memarg),    // i32.store
    I64Store(Memarg),    // i64.store
    F32Store(Memarg),    // f32.store
    F64Store(Memarg),    // f64.store
    I32Store8(Memarg),   // i32.store8
    I32Store16(Memarg),  // i32.store16
    I64Store8(Memarg),   // i64.store8
    I64Store16(Memarg),  // i64.store16
    I64Store32(Memarg),  // i64.store32
    MemorySize,          // memory.size
    MemoryGrow,          // memory.grow
    MemoryInit(DataIdx), // memory.init
    DataDrop(DataIdx),   // data.drop
    MemoryCopy,          // memory.copy
    MemoryFill,          // memory.fill

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Memarg {
    pub align: u32,
    pub offset: u32,
}

impl Instr {
    pub fn parse(mut input: impl io::Read) -> Result<Option<Self>> {
        let mut buf = [0u8];

        let _ = match input.read_exact(&mut buf) {
            Ok(_) => Ok::<(), anyhow::Error>(()),
            //    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
            Err(e) => return Err(e.into()),
        };

        let ins = match buf[0] {
            // opcode for `end` - not technically an instruction
            0x0B => return Ok(None),

            // --- Control instructions (5.4.1) ---
            // ...

            // --- Reference instructions (5.4.2) ---
            0xD0 => Instr::RefNull(RefType::read(&mut input)?),
            0xD1 => Instr::RefIsNull,
            0xD2 => Instr::RefFunc(FuncIdx::read(input)?),

            // --- Parametric instructions (5.4.3) ---
            0x1A => Instr::Drop,
            0x1B => Instr::Select(None),
            0x1C => Instr::Select(Some(parse_vec(&mut input, parse_valtype)?)),

            // --- Variable instructions (5.4.4) ---
            0x20 => Instr::LocalGet(LocalIdx::read(input)?),
            0x21 => Instr::LocalSet(LocalIdx::read(input)?),
            0x22 => Instr::LocalTee(LocalIdx::read(input)?),
            0x23 => Instr::GlobalGet(GlobalIdx::read(input)?),
            0x24 => Instr::GlobalSet(GlobalIdx::read(input)?),

            // --- Table instructions (5.4.5) ---
            0x25 => Instr::TableGet(TableIdx::read(input)?),
            0x26 => Instr::TableSet(TableIdx::read(input)?),
            // ...the rest are below, together with some Memory instructions
            // since they both share a common opcode - 0xFC

            // --- Memory instructions (5.4.6) ---
            op @ 0x28..=0x3E => {
                let m = Memarg {
                    align: parse_u32(&mut input)?,
                    offset: parse_u32(input)?,
                };
                match op {
                    0x28 => Instr::I32Load(m),
                    0x29 => Instr::I64Load(m),
                    0x2A => Instr::F32Load(m),
                    0x2B => Instr::F64Load(m),
                    0x2C => Instr::I32Load8s(m),
                    0x2D => Instr::I32Load8u(m),
                    0x2E => Instr::I32Load16s(m),
                    0x2F => Instr::I32Load16u(m),
                    0x30 => Instr::I64Load8s(m),
                    0x31 => Instr::I64Load8u(m),
                    0x32 => Instr::I64Load16s(m),
                    0x33 => Instr::I64Load16u(m),
                    0x34 => Instr::I64Load32s(m),
                    0x35 => Instr::I64Load32u(m),
                    0x36 => Instr::I32Store(m),
                    0x37 => Instr::I64Store(m),
                    0x38 => Instr::F32Store(m),
                    0x39 => Instr::F64Store(m),
                    0x3A => Instr::I32Store8(m),
                    0x3B => Instr::I32Store16(m),
                    0x3C => Instr::I64Store8(m),
                    0x3D => Instr::I64Store16(m),
                    0x3E => Instr::I64Store32(m),
                    n => return Err(anyhow!("unexpected memory instruction {:X}", n)),
                }
            }
            0x3F => {
                let b = read_byte(input)?;
                if b != 0x00 {
                    return Err(anyhow!("unexpected byte {:X} for memory.size", b));
                }
                Instr::MemorySize
            }
            0x40 => {
                let b = read_byte(input)?;
                if b != 0x00 {
                    return Err(anyhow!("unexpected byte {:X} for memory.grow", b));
                }
                Instr::MemoryGrow
            }
            // ...the rest are below, together with table since they both share
            // a common opcode - 0xFC

            // --- Table / Memory instructions ---
            0xFC => match parse_u32(&mut input)? {
                // --- memory ---
                8 => {
                    let x = DataIdx::read(&mut input)?;
                    let b = read_byte(input)?;
                    if b != 0x00 {
                        return Err(anyhow!("unexpected byte {:X} for memory.init", b));
                    }
                    Instr::MemoryInit(x)
                }
                9 => Instr::DataDrop(DataIdx::read(input)?),
                10 => {
                    let mut buf = [0u8; 2];
                    input.read_exact(&mut buf)?;
                    if buf != [0u8, 0u8] {
                        return Err(anyhow!("unexpected bytes {:?} for memory.copy", buf));
                    }
                    Instr::MemoryCopy
                }
                11 => {
                    let b = read_byte(input)?;
                    if b != 0x00 {
                        return Err(anyhow!("unexpected byte {:X} for memory.fill", b));
                    }
                    Instr::MemoryFill
                }

                // --- table ---
                12 => {
                    let y = ElemIdx::read(&mut input)?;
                    let x = TableIdx::read(&mut input)?;
                    Instr::TableInit(x, y)
                }
                13 => Instr::ElemDrop(ElemIdx::read(input)?),
                14 => Instr::TableCopy(TableIdx::read(&mut input)?, TableIdx::read(input)?),
                15 => Instr::TableGrow(TableIdx::read(input)?),
                16 => Instr::TableSize(TableIdx::read(input)?),
                17 => Instr::TableFill(TableIdx::read(input)?),
                n => return Err(anyhow!("unexpected table instr prefix byte `{:x}`", n)),
            },

            // --- Numeric instructions (5.4.7) ---
            0x41 => Instr::I32Const(parse_i32(input)?),
            0x42 => Instr::I64Const(parse_i64(input)?),
            0x6A => Instr::I32Add,
            // ...TODO
            n => {
                return Err(anyhow!("unexpected instr: {:#X}", n));
            }
        };

        Ok(Some(ins))
    }
}
