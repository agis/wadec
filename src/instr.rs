use crate::*;
use std::io::{Cursor, Read};

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    // --- Control instructions (5.4.1) ---
    Unreachable,
    Nop,
    Block(BlockType, Vec<Instr>),
    Loop(BlockType, Vec<Instr>),
    If(BlockType, Vec<Instr>, Option<Vec<Instr>>),
    Br(LabelIdx),
    BrIf(LabelIdx),
    BrTable(Vec<LabelIdx>, LabelIdx),
    Return,
    Call(FuncIdx),
    CallIndirect(TableIdx, TypeIdx),

    // --- Reference instructions (5.4.2) ---
    RefNull(RefType),
    RefIsNull,
    RefFunc(FuncIdx),

    // --- Parametric instructions (5.4.3) ---
    Drop,
    Select(Option<Vec<ValType>>),

    // --- Variable instructions (5.4.4) ---
    LocalGet(LocalIdx),
    LocalSet(LocalIdx),
    LocalTee(LocalIdx),
    GlobalGet(GlobalIdx),
    GlobalSet(GlobalIdx),

    // --- Table instructions (5.4.5) ---
    TableGet(TableIdx),
    TableSet(TableIdx),
    TableInit(TableIdx, ElemIdx),
    ElemDrop(ElemIdx),
    TableCopy(TableIdx, TableIdx),
    TableGrow(TableIdx),
    TableSize(TableIdx),
    TableFill(TableIdx),

    // --- Memory instructions (5.4.6) ---
    I32Load(Memarg),
    I64Load(Memarg),
    F32Load(Memarg),
    F64Load(Memarg),
    I32Load8s(Memarg),
    I32Load8u(Memarg),
    I32Load16s(Memarg),
    I32Load16u(Memarg),
    I64Load8s(Memarg),
    I64Load8u(Memarg),
    I64Load16s(Memarg),
    I64Load16u(Memarg),
    I64Load32s(Memarg),
    I64Load32u(Memarg),
    I32Store(Memarg),
    I64Store(Memarg),
    F32Store(Memarg),
    F64Store(Memarg),
    I32Store8(Memarg),
    I32Store16(Memarg),
    I64Store8(Memarg),
    I64Store16(Memarg),
    I64Store32(Memarg),
    MemorySize,
    MemoryGrow,
    MemoryInit(DataIdx),
    DataDrop(DataIdx),
    MemoryCopy,
    MemoryFill,

    // --- Numeric constants (5.4.7) ---
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),

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

impl Instr {
    pub fn parse<R: io::Read>(reader: &mut R) -> Result<Parsed> {
        let mut buf = [0u8];

        let _ = match reader.read_exact(&mut buf) {
            Ok(_) => Ok::<(), anyhow::Error>(()),
            Err(e) => return Err(e.into()),
        };

        let ins = match buf[0] {
            0x0B => return Ok(Parsed::End),
            0x05 => return Ok(Parsed::Else),

            // --- Control instructions (5.4.1) ---
            0x00 => Instr::Unreachable,
            0x01 => Instr::Nop,
            op @ 0x02..=0x04 => {
                let bt = BlockType::read(reader)?;
                let mut in1 = Vec::new();
                let mut parsed;

                loop {
                    parsed = Self::parse(reader)?;
                    match parsed {
                        Parsed::Instr(i) => in1.push(i),
                        Parsed::End | Parsed::Else => break,
                    }
                }

                match op {
                    0x02 => Instr::Block(bt, in1),
                    0x03 => Instr::Loop(bt, in1),
                    0x04 => match parsed {
                        Parsed::End => Instr::If(bt, in1, None),
                        Parsed::Else => {
                            let mut in2 = Vec::new();
                            loop {
                                parsed = Self::parse(reader)?;
                                match parsed {
                                    Parsed::Instr(i) => in2.push(i),
                                    Parsed::End => break,
                                    _ => bail!("unexpected `Else`"),
                                }
                            }
                            Instr::If(bt, in1, Some(in2))
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            0x0C => Instr::Br(LabelIdx::read(reader)?),
            0x0D => Instr::BrIf(LabelIdx::read(reader)?),
            0x0E => {
                let l = parse_vec(reader, LabelIdx::read)?;
                let ln = LabelIdx::read(reader)?;
                Instr::BrTable(l, ln)
            }
            0x0F => Instr::Return,
            0x10 => Instr::Call(FuncIdx::read(reader)?),
            0x11 => {
                let y = TypeIdx::read(reader)?;
                let x = TableIdx::read(reader)?;
                Instr::CallIndirect(x, y)
            }

            // --- Reference instructions (5.4.2) ---
            0xD0 => Instr::RefNull(RefType::read(reader)?),
            0xD1 => Instr::RefIsNull,
            0xD2 => Instr::RefFunc(FuncIdx::read(reader)?),

            // --- Parametric instructions (5.4.3) ---
            0x1A => Instr::Drop,
            0x1B => Instr::Select(None),
            0x1C => Instr::Select(Some(parse_vec(reader, ValType::read)?)),

            // --- Variable instructions (5.4.4) ---
            0x20 => Instr::LocalGet(LocalIdx::read(reader)?),
            0x21 => Instr::LocalSet(LocalIdx::read(reader)?),
            0x22 => Instr::LocalTee(LocalIdx::read(reader)?),
            0x23 => Instr::GlobalGet(GlobalIdx::read(reader)?),
            0x24 => Instr::GlobalSet(GlobalIdx::read(reader)?),

            // --- Table instructions (5.4.5) ---
            0x25 => Instr::TableGet(TableIdx::read(reader)?),
            0x26 => Instr::TableSet(TableIdx::read(reader)?),
            // ...the rest are below, together with some Memory instructions
            // since they both share a common opcode - 0xFC

            // --- Memory instructions (5.4.6) ---
            op @ 0x28..=0x3E => {
                let m = Memarg {
                    align: parse_u32(reader)?,
                    offset: parse_u32(reader)?,
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
                    n => bail!("unexpected memory instruction {:X}", n),
                }
            }
            0x3F => {
                let b = read_byte(reader)?;
                if b != 0x00 {
                    bail!("unexpected byte {:X} for memory.size", b);
                }
                Instr::MemorySize
            }
            0x40 => {
                let b = read_byte(reader)?;
                if b != 0x00 {
                    bail!("unexpected byte {:X} for memory.grow", b);
                }
                Instr::MemoryGrow
            }
            // ...the rest are below, together with table since they both share
            // a common opcode - 0xFC

            // --- Table / Memory instructions ---
            0xFC => match parse_u32(reader)? {
                // --- memory ---
                8 => {
                    let x = DataIdx::read(reader)?;
                    let b = read_byte(reader)?;
                    if b != 0x00 {
                        bail!("unexpected byte {:X} for memory.init", b);
                    }
                    Instr::MemoryInit(x)
                }
                9 => Instr::DataDrop(DataIdx::read(reader)?),
                10 => {
                    let mut buf = [0u8; 2];
                    reader.read_exact(&mut buf)?;
                    if buf != [0u8, 0u8] {
                        bail!("unexpected bytes {:?} for memory.copy", buf);
                    }
                    Instr::MemoryCopy
                }
                11 => {
                    let b = read_byte(reader)?;
                    if b != 0x00 {
                        bail!("unexpected byte {:X} for memory.fill", b);
                    }
                    Instr::MemoryFill
                }

                // --- table ---
                12 => {
                    let y = ElemIdx::read(reader)?;
                    let x = TableIdx::read(reader)?;
                    Instr::TableInit(x, y)
                }
                13 => Instr::ElemDrop(ElemIdx::read(reader)?),
                14 => Instr::TableCopy(TableIdx::read(reader)?, TableIdx::read(reader)?),
                15 => Instr::TableGrow(TableIdx::read(reader)?),
                16 => Instr::TableSize(TableIdx::read(reader)?),
                17 => Instr::TableFill(TableIdx::read(reader)?),
                n => bail!("unexpected table instr prefix byte `{:x}`", n),
            },

            // --- Numeric instructions (5.4.7) ---
            0x41 => Instr::I32Const(parse_i32(reader)?),
            0x42 => Instr::I64Const(parse_i64(reader)?),
            0x6A => Instr::I32Add,
            // ...TODO
            n => bail!("unexpected instr: {:#X}", n),
        };

        Ok(Parsed::Instr(ins))
    }
}

pub enum Parsed {
    Instr(Instr),
    Else,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Memarg {
    pub align: u32,
    pub offset: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockType {
    Empty,
    T(ValType),
    X(u32),
}

impl BlockType {
    fn read<R: io::Read>(reader: &mut R) -> Result<BlockType> {
        let b = read_byte(reader)?;

        if b == 0x40 {
            return Ok(BlockType::Empty);
        }

        if let Ok(t) = ValType::try_from(b) {
            return Ok(BlockType::T(t));
        }

        let mut chain = Cursor::new([b]).chain(reader);
        let x = parse_i64(&mut chain)?;
        if x < 0 {
            bail!("blocktype integer negative");
        }

        Ok(BlockType::X(x.try_into()?))
    }
}
