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
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
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
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,
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

            // --- Numeric instructions (5.4.7) ---
            0x41 => Instr::I32Const(parse_i32(reader)?),
            0x42 => Instr::I64Const(parse_i64(reader)?),
            0x43 => Instr::F32Const(parse_f32(reader)?),
            0x44 => Instr::F64Const(parse_f64(reader)?),
            0x45 => Instr::I32Eqz,
            0x46 => Instr::I32Eq,
            0x47 => Instr::I32Ne,
            0x48 => Instr::I32LtS,
            0x49 => Instr::I32LtU,
            0x4A => Instr::I32GtS,
            0x4B => Instr::I32GtU,
            0x4C => Instr::I32LeS,
            0x4D => Instr::I32LeU,
            0x4E => Instr::I32GeS,
            0x4F => Instr::I32GeU,
            0x50 => Instr::I64Eqz,
            0x51 => Instr::I64Eq,
            0x52 => Instr::I64Ne,
            0x53 => Instr::I64LtS,
            0x54 => Instr::I64LtU,
            0x55 => Instr::I64GtS,
            0x56 => Instr::I64GtU,
            0x57 => Instr::I64LeS,
            0x58 => Instr::I64LeU,
            0x59 => Instr::I64GeS,
            0x5A => Instr::I64GeU,
            0x5B => Instr::F32Eq,
            0x5C => Instr::F32Ne,
            0x5D => Instr::F32Lt,
            0x5E => Instr::F32Gt,
            0x5F => Instr::F32Le,
            0x60 => Instr::F32Ge,
            0x61 => Instr::F64Eq,
            0x62 => Instr::F64Ne,
            0x63 => Instr::F64Lt,
            0x64 => Instr::F64Gt,
            0x65 => Instr::F64Le,
            0x66 => Instr::F64Ge,
            0x67 => Instr::I32Clz,
            0x68 => Instr::I32Ctz,
            0x69 => Instr::I32Popcnt,
            0x6A => Instr::I32Add,
            0x6B => Instr::I32Sub,
            0x6C => Instr::I32Mul,
            0x6D => Instr::I32DivS,
            0x6E => Instr::I32DivU,
            0x6F => Instr::I32RemS,
            0x70 => Instr::I32RemU,
            0x71 => Instr::I32And,
            0x72 => Instr::I32Or,
            0x73 => Instr::I32Xor,
            0x74 => Instr::I32Shl,
            0x75 => Instr::I32ShrS,
            0x76 => Instr::I32ShrU,
            0x77 => Instr::I32Rotl,
            0x78 => Instr::I32Rotr,
            0x79 => Instr::I64Clz,
            0x7A => Instr::I64Ctz,
            0x7B => Instr::I64Popcnt,
            0x7C => Instr::I64Add,
            0x7D => Instr::I64Sub,
            0x7E => Instr::I64Mul,
            0x7F => Instr::I64DivS,
            0x80 => Instr::I64DivU,
            0x81 => Instr::I64RemS,
            0x82 => Instr::I64RemU,
            0x83 => Instr::I64And,
            0x84 => Instr::I64Or,
            0x85 => Instr::I64Xor,
            0x86 => Instr::I64Shl,
            0x87 => Instr::I64ShrS,
            0x88 => Instr::I64ShrU,
            0x89 => Instr::I64Rotl,
            0x8A => Instr::I64Rotr,
            0x8B => Instr::F32Abs,
            0x8C => Instr::F32Neg,
            0x8D => Instr::F32Ceil,
            0x8E => Instr::F32Floor,
            0x8F => Instr::F32Trunc,
            0x90 => Instr::F32Nearest,
            0x91 => Instr::F32Sqrt,
            0x92 => Instr::F32Add,
            0x93 => Instr::F32Sub,
            0x94 => Instr::F32Mul,
            0x95 => Instr::F32Div,
            0x96 => Instr::F32Min,
            0x97 => Instr::F32Max,
            0x98 => Instr::F32Copysign,
            0x99 => Instr::F64Abs,
            0x9A => Instr::F64Neg,
            0x9B => Instr::F64Ceil,
            0x9C => Instr::F64Floor,
            0x9D => Instr::F64Trunc,
            0x9E => Instr::F64Nearest,
            0x9F => Instr::F64Sqrt,
            0xA0 => Instr::F64Add,
            0xA1 => Instr::F64Sub,
            0xA2 => Instr::F64Mul,
            0xA3 => Instr::F64Div,
            0xA4 => Instr::F64Min,
            0xA5 => Instr::F64Max,
            0xA6 => Instr::F64Copysign,
            0xA7 => Instr::I32WrapI64,
            0xA8 => Instr::I32TruncF32S,
            0xA9 => Instr::I32TruncF32U,
            0xAA => Instr::I32TruncF64S,
            0xAB => Instr::I32TruncF64U,
            0xAC => Instr::I64ExtendI32S,
            0xAD => Instr::I64ExtendI32U,
            0xAE => Instr::I64TruncF32S,
            0xAF => Instr::I64TruncF32U,
            0xB0 => Instr::I64TruncF64S,
            0xB1 => Instr::I64TruncF64U,
            0xB2 => Instr::F32ConvertI32S,
            0xB3 => Instr::F32ConvertI32U,
            0xB4 => Instr::F32ConvertI64S,
            0xB5 => Instr::F32ConvertI64U,
            0xB6 => Instr::F32DemoteF64,
            0xB7 => Instr::F64ConvertI32S,
            0xB8 => Instr::F64ConvertI32U,
            0xB9 => Instr::F64ConvertI64S,
            0xBA => Instr::F64ConvertI64U,
            0xBB => Instr::F64PromoteF32,
            0xBC => Instr::I32ReinterpretF32,
            0xBD => Instr::I64ReinterpretF64,
            0xBE => Instr::F32ReinterpretI32,
            0xBF => Instr::F64ReinterpretI64,
            0xC0 => Instr::I32Extend8S,
            0xC1 => Instr::I32Extend16S,
            0xC2 => Instr::I64Extend8S,
            0xC3 => Instr::I64Extend16S,
            0xC4 => Instr::I64Extend32S,

            // 0xFC is shared by Table, Memory and Numeric instructions
            0xFC => match parse_u32(reader)? {
                // --- Numeric saturating truncation ---
                0 => Instr::I32TruncSatF32S,
                1 => Instr::I32TruncSatF32U,
                2 => Instr::I32TruncSatF64S,
                3 => Instr::I32TruncSatF64U,
                4 => Instr::I64TruncSatF32S,
                5 => Instr::I64TruncSatF32U,
                6 => Instr::I64TruncSatF64S,
                7 => Instr::I64TruncSatF64U,

                // --- Memory ---
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

                // --- Table ---
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
