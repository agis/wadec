//! WebAssembly instruction decoding.
//!
//! Defined in <https://www.w3.org/TR/wasm-core-2/#instructions>
use crate::core::indices::*;
use crate::core::instruction::{BlockType, Catch, Instruction, LaneIdx, Memarg};
use crate::core::types::{heaptype::HeapType, reftype::RefType, valtype::ValType};
use crate::decode::helpers::{decode_f32, decode_f64, decode_list};
use crate::decode::helpers::{DecodeFloat32Error, DecodeFloat64Error, DecodeListError};
use crate::decode::indices::*;
use crate::decode::integer::{
    decode_i32, decode_i64, decode_s33, decode_u32, decode_u64, DecodeI32Error, DecodeI64Error,
    DecodeS33Error, DecodeU32Error, DecodeU64Error,
};
use crate::decode::types::{DecodeHeapTypeError, DecodeValTypeError};
use crate::read_byte;
use std::io::{self, Cursor, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("failed reading instruction opcode")]
    ReadOpcode(#[from] io::Error),

    #[error("failed decoding control instruction")]
    Control(#[from] ControlError),

    #[error("failed decoding reference instruction")]
    Reference(#[from] ReferenceError),

    #[error("failed decoding aggregate instruction")]
    Aggregate(#[from] AggregateError),

    #[error("failed decoding parametric instruction")]
    Parametric(#[from] ParametricError),

    #[error("failed decoding variable instruction")]
    Variable(#[from] VariableError),

    #[error("failed decoding table instruction")]
    Table(#[from] TableError),

    #[error("failed decoding memory instruction")]
    Memory(#[from] MemoryError),

    #[error("failed decoding numeric instruction")]
    Numeric(#[from] NumericError),

    #[error("failed decoding vector instruction")]
    Vector(#[from] VectorError),

    #[error("unexpected opcode: {0:#04X}")]
    InvalidOpcode(u8),

    #[error("unexpected opcode after 0xFC marker byte: {0:#04X}")]
    InvalidMarkerByteAfterFC(u32),

    #[error("unexpected opcode after 0xFB marker byte: {0:#04X}")]
    InvalidMarkerByteAfterFB(u32),
}

#[derive(Debug, Error)]
pub enum ControlError {
    #[error("failed decoding label index")]
    LabelIdx(#[from] DecodeLabelIdxError),

    #[error("failed decoding label index")]
    DecodeLabelIdxVector(#[from] DecodeListError<DecodeLabelIdxError>),

    #[error("failed decoding function index")]
    FuncIdx(DecodeFuncIdxError),

    #[error("failed decoding table index")]
    TableIdx(DecodeTableIdxError),

    #[error("failed decoding type index")]
    TypeIdx(DecodeTypeIdxError),

    #[error("failed decoding tag index")]
    TagIdx(DecodeTagIdxError),

    #[error("failed decoding Catch list")]
    DecodeCatchListError(#[from] DecodeListError<CatchError>),

    #[error("failed decoding block type")]
    BlockType(BlockTypeError),

    #[error("unexpected `Else` token")]
    UnexpectedElse,

    #[error("failed decoding nullability for cast operation")]
    ReadCastNullabilityMarker(io::Error),

    #[error("invalid cast operation marker byte: expected 0x00, 0x01, 0x02 or 0x03; got {0:#04X}")]
    InvalidCastNullabilityMarker(u8),

    #[error("failed decoding heap type for cast operation")]
    HeapType(#[from] DecodeHeapTypeError),
}

#[derive(Debug, Error)]
pub enum ReferenceError {
    #[error("failed decoding heap type")]
    HeapType(DecodeHeapTypeError),

    #[error("failed decoding function index")]
    FuncIdx(DecodeFuncIdxError),

    #[error("failed reading sub-opcode")]
    ReadSubOpcode(DecodeU32Error),

    #[error("failed decoding sub-opcode")]
    InvalidSubOpcode(u32),
}

#[derive(Debug, Error)]
pub enum AggregateError {
    #[error("failed decoding type index")]
    TypeIdx(DecodeTypeIdxError),

    #[error("failed decoding field index")]
    FieldIndex(DecodeU32Error),

    #[error("failed decoding data index")]
    DataIdx(DecodeDataIdxError),

    #[error("failed decoding element index")]
    ElemIdx(DecodeElemIdxError),

    #[error("failed decoding array fixed length")]
    ArrayFixedLength(DecodeU32Error),
}

#[derive(Debug, Error)]
pub enum ParametricError {
    #[error("failed decoding value type vector")]
    DecodeVector(#[from] DecodeListError<DecodeValTypeError>),
}

#[derive(Debug, Error)]
pub enum VariableError {
    #[error("failed decoding Local index")]
    LocalIdx(DecodeLocalIdxError),

    #[error("failed decoding Global index")]
    GlobalIdx(DecodeGlobalIdxError),
}

#[derive(Debug, Error)]
pub enum TableError {
    #[error("failed decoding table index")]
    TableIdx(DecodeTableIdxError),

    #[error("failed decoding element index")]
    ElemIdx(DecodeElemIdxError),
}

#[derive(Debug, Error)]
pub enum MemoryError {
    #[error("failed decoding Memarg")]
    DecodeMemarg(MemargError),

    #[error("failed decoding data index")]
    DecodeDataIdx(DecodeDataIdxError),

    #[error("failed decoding memory index")]
    DecodeMemIdx(DecodeMemIdxError),
}

#[derive(Debug, Error)]
pub enum NumericError {
    #[error("failed reading 0xFC-prefixed numeric opcode")]
    ReadOpcode(DecodeU32Error),

    #[error(transparent)]
    DecodeI32(DecodeI32Error),

    #[error(transparent)]
    DecodeI64(DecodeI64Error),

    #[error(transparent)]
    DecodeF32(DecodeFloat32Error),

    #[error(transparent)]
    DecodeF64(DecodeFloat64Error),
}

#[derive(Debug, Error)]
pub enum VectorError {
    #[error("failed reading Vector opcode")]
    ReadOpcode(DecodeU32Error),

    #[error(transparent)]
    Memarg(MemargError),

    #[error(transparent)]
    LaneIdx(LaneIdxError),

    #[error("failed reading immediate bytes")]
    ReadImmediateBytes(io::Error),

    #[error("unexpected Vector opcode: {0:#04X}")]
    InvalidOpcode(u32),
}

impl Instruction {
    pub(crate) fn parse<R: Read + ?Sized>(reader: &mut R) -> Result<ParseResult, ParseError> {
        let mut opcode = [0u8];
        reader.read_exact(&mut opcode)?;

        let ins = match opcode[0] {
            0x0B => return Ok(ParseResult::End),
            0x05 => return Ok(ParseResult::Else),

            // --- Parametric instructions ---
            0x00 => Instruction::Unreachable,
            0x01 => Instruction::Nop,
            0x1A => Instruction::Drop,
            0x1B => Instruction::Select(None),
            0x1C => Instruction::Select(Some(
                decode_list(reader, ValType::decode).map_err(ParametricError::DecodeVector)?,
            )),

            // --- Control instructions ---
            op @ 0x02..=0x04 => {
                let bt = BlockType::decode(reader).map_err(ControlError::BlockType)?;
                let mut in1 = Vec::new();
                let mut parsed;

                loop {
                    parsed = Self::parse(reader)?;
                    match parsed {
                        ParseResult::Instruction(i) => in1.push(i),
                        ParseResult::End | ParseResult::Else => break,
                    }
                }

                match op {
                    0x02 => Instruction::Block(bt, in1),
                    0x03 => Instruction::Loop(bt, in1),
                    0x04 => match parsed {
                        ParseResult::End => Instruction::If(bt, in1, None),
                        ParseResult::Else => {
                            let mut in2 = Vec::new();
                            loop {
                                parsed = Self::parse(reader)?;
                                match parsed {
                                    ParseResult::Instruction(i) => in2.push(i),
                                    ParseResult::End => break,
                                    _ => return Err(ControlError::UnexpectedElse.into()),
                                }
                            }
                            Instruction::If(bt, in1, Some(in2))
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            0x08 => Instruction::Throw(TagIdx::decode(reader).map_err(ControlError::TagIdx)?),
            0x0A => Instruction::ThrowRef,
            0x0C => Instruction::Br(LabelIdx::decode(reader).map_err(ControlError::LabelIdx)?),
            0x0D => Instruction::BrIf(LabelIdx::decode(reader).map_err(ControlError::LabelIdx)?),
            0x0E => {
                let l = decode_list(reader, LabelIdx::decode)
                    .map_err(ControlError::DecodeLabelIdxVector)?;
                let ln = LabelIdx::decode(reader).map_err(ControlError::LabelIdx)?;
                Instruction::BrTable(l, ln)
            }
            0x0F => Instruction::Return,
            0x10 => Instruction::Call(FuncIdx::decode(reader).map_err(ControlError::FuncIdx)?),
            0x11 => {
                let y = TypeIdx::decode(reader).map_err(ControlError::TypeIdx)?;
                let x = TableIdx::decode(reader).map_err(ControlError::TableIdx)?;
                Instruction::CallIndirect(x, y)
            }
            0x12 => {
                Instruction::ReturnCall(FuncIdx::decode(reader).map_err(ControlError::FuncIdx)?)
            }
            0x13 => {
                let y = TypeIdx::decode(reader).map_err(ControlError::TypeIdx)?;
                let x = TableIdx::decode(reader).map_err(ControlError::TableIdx)?;
                Instruction::ReturnCallIndirect(x, y)
            }
            0x14 => Instruction::CallRef(TypeIdx::decode(reader).map_err(ControlError::TypeIdx)?),
            0x15 => {
                Instruction::ReturnCallRef(TypeIdx::decode(reader).map_err(ControlError::TypeIdx)?)
            }
            0x1F => {
                let bt = BlockType::decode(reader).map_err(ControlError::BlockType)?;
                let c = decode_list(reader, Catch::decode)
                    .map_err(ControlError::DecodeCatchListError)?;

                let mut ins = Vec::new();
                let mut parsed;
                loop {
                    parsed = Self::parse(reader)?;
                    match parsed {
                        ParseResult::Instruction(i) => ins.push(i),
                        ParseResult::End | ParseResult::Else => break,
                    }
                }

                Instruction::TryTable(bt, c, ins)
            }
            0xD5 => {
                Instruction::BrOnNull(LabelIdx::decode(reader).map_err(ControlError::LabelIdx)?)
            }
            0xD6 => {
                Instruction::BrOnNonNull(LabelIdx::decode(reader).map_err(ControlError::LabelIdx)?)
            }
            // NOTE: 0xFB is grouped further below, since it's common among Control and Reference
            // instructions

            // --- Reference instructions (5.4.6) ---
            0xD0 => {
                Instruction::RefNull(HeapType::decode(reader).map_err(ReferenceError::HeapType)?)
            }
            0xD1 => Instruction::RefIsNull,
            0xD2 => Instruction::RefFunc(FuncIdx::decode(reader).map_err(ReferenceError::FuncIdx)?),
            0xD3 => Instruction::RefEq,
            0xD4 => Instruction::RefAsNonNull,

            // 0xFB prefix is shared among Control, Reference and Aggregate instructions
            0xFB => {
                let op = decode_u32(reader).map_err(ReferenceError::ReadSubOpcode)?;
                match op {
                    // Aggregate
                    0 => Instruction::StructNew(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    1 => Instruction::StructNewDefault(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    2 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let f = decode_u32(reader).map_err(AggregateError::FieldIndex)?;
                        Instruction::StructGet(t, f)
                    }
                    3 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let f = decode_u32(reader).map_err(AggregateError::FieldIndex)?;
                        Instruction::StructGetS(t, f)
                    }
                    4 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let f = decode_u32(reader).map_err(AggregateError::FieldIndex)?;
                        Instruction::StructGetU(t, f)
                    }
                    5 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let f = decode_u32(reader).map_err(AggregateError::FieldIndex)?;
                        Instruction::StructSet(t, f)
                    }
                    6 => Instruction::ArrayNew(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    7 => Instruction::ArrayNewDefault(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    8 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let n = decode_u32(reader).map_err(AggregateError::ArrayFixedLength)?;
                        Instruction::ArrayNewFixed(t, n)
                    }
                    9 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let d = DataIdx::decode(reader).map_err(AggregateError::DataIdx)?;
                        Instruction::ArrayNewData(t, d)
                    }
                    10 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let e = ElemIdx::decode(reader).map_err(AggregateError::ElemIdx)?;
                        Instruction::ArrayNewElem(t, e)
                    }
                    11 => Instruction::ArrayGet(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    12 => Instruction::ArrayGetS(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    13 => Instruction::ArrayGetU(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    14 => Instruction::ArraySet(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    15 => Instruction::ArrayLen,
                    16 => Instruction::ArrayFill(
                        TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?,
                    ),
                    17 => {
                        let dst = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let src = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        Instruction::ArrayCopy(dst, src)
                    }
                    18 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let d = DataIdx::decode(reader).map_err(AggregateError::DataIdx)?;
                        Instruction::ArrayInitData(t, d)
                    }
                    19 => {
                        let t = TypeIdx::decode(reader).map_err(AggregateError::TypeIdx)?;
                        let e = ElemIdx::decode(reader).map_err(AggregateError::ElemIdx)?;
                        Instruction::ArrayInitElem(t, e)
                    }

                    // Reference
                    20..=23 => {
                        let ht = HeapType::decode(reader).map_err(ReferenceError::HeapType)?;
                        match op {
                            20 => Instruction::RefTest(RefType {
                                nullable: false,
                                ht,
                            }),
                            21 => Instruction::RefTest(RefType { nullable: true, ht }),
                            22 => Instruction::RefCast(RefType {
                                nullable: false,
                                ht,
                            }),
                            23 => Instruction::RefCast(RefType { nullable: true, ht }),
                            _ => unreachable!(),
                        }
                    }

                    // Control
                    24 | 25 => {
                        let (nullable1, nullable2) = decode_castop(reader)?;
                        let l = LabelIdx::decode(reader).map_err(ControlError::LabelIdx)?;
                        let ht1 = HeapType::decode(reader).map_err(ControlError::HeapType)?;
                        let ht2 = HeapType::decode(reader).map_err(ControlError::HeapType)?;
                        let rt1 = RefType {
                            nullable: nullable1,
                            ht: ht1,
                        };
                        let rt2 = RefType {
                            nullable: nullable2,
                            ht: ht2,
                        };

                        if op == 24 {
                            Instruction::BrOnCast(l, rt1, rt2)
                        } else {
                            Instruction::BrOnCastFail(l, rt1, rt2)
                        }
                    }

                    26 => Instruction::AnyConvertExtern,
                    27 => Instruction::ExternConvertAny,
                    28 => Instruction::RefI31,
                    29 => Instruction::I31GetS,
                    30 => Instruction::I31GetU,
                    n => return Err(ParseError::InvalidMarkerByteAfterFB(n)),
                }
            }

            // --- Variable instructions (5.4.4) ---
            0x20 => {
                Instruction::LocalGet(LocalIdx::decode(reader).map_err(VariableError::LocalIdx)?)
            }
            0x21 => {
                Instruction::LocalSet(LocalIdx::decode(reader).map_err(VariableError::LocalIdx)?)
            }
            0x22 => {
                Instruction::LocalTee(LocalIdx::decode(reader).map_err(VariableError::LocalIdx)?)
            }
            0x23 => {
                Instruction::GlobalGet(GlobalIdx::decode(reader).map_err(VariableError::GlobalIdx)?)
            }
            0x24 => {
                Instruction::GlobalSet(GlobalIdx::decode(reader).map_err(VariableError::GlobalIdx)?)
            }

            // --- Table instructions (5.4.5) ---
            0x25 => Instruction::TableGet(TableIdx::decode(reader).map_err(TableError::TableIdx)?),
            0x26 => Instruction::TableSet(TableIdx::decode(reader).map_err(TableError::TableIdx)?),

            // --- Memory instructions (5.4.6) ---
            op @ 0x28..=0x3E => {
                let m = Memarg::decode(reader).map_err(MemoryError::DecodeMemarg)?;
                match op {
                    0x28 => Instruction::I32Load(m),
                    0x29 => Instruction::I64Load(m),
                    0x2A => Instruction::F32Load(m),
                    0x2B => Instruction::F64Load(m),
                    0x2C => Instruction::I32Load8s(m),
                    0x2D => Instruction::I32Load8u(m),
                    0x2E => Instruction::I32Load16s(m),
                    0x2F => Instruction::I32Load16u(m),
                    0x30 => Instruction::I64Load8s(m),
                    0x31 => Instruction::I64Load8u(m),
                    0x32 => Instruction::I64Load16s(m),
                    0x33 => Instruction::I64Load16u(m),
                    0x34 => Instruction::I64Load32s(m),
                    0x35 => Instruction::I64Load32u(m),
                    0x36 => Instruction::I32Store(m),
                    0x37 => Instruction::I64Store(m),
                    0x38 => Instruction::F32Store(m),
                    0x39 => Instruction::F64Store(m),
                    0x3A => Instruction::I32Store8(m),
                    0x3B => Instruction::I32Store16(m),
                    0x3C => Instruction::I64Store8(m),
                    0x3D => Instruction::I64Store16(m),
                    0x3E => Instruction::I64Store32(m),
                    _ => unreachable!(),
                }
            }
            0x3F => {
                Instruction::MemorySize(MemIdx::decode(reader).map_err(MemoryError::DecodeMemIdx)?)
            }

            0x40 => {
                Instruction::MemoryGrow(MemIdx::decode(reader).map_err(MemoryError::DecodeMemIdx)?)
            }

            // --- Numeric instructions (5.4.7) ---
            0x41 => Instruction::I32Const(decode_i32(reader).map_err(NumericError::DecodeI32)?),
            0x42 => Instruction::I64Const(decode_i64(reader).map_err(NumericError::DecodeI64)?),
            0x43 => Instruction::F32Const(decode_f32(reader).map_err(NumericError::DecodeF32)?),
            0x44 => Instruction::F64Const(decode_f64(reader).map_err(NumericError::DecodeF64)?),
            0x45 => Instruction::I32Eqz,
            0x46 => Instruction::I32Eq,
            0x47 => Instruction::I32Ne,
            0x48 => Instruction::I32LtS,
            0x49 => Instruction::I32LtU,
            0x4A => Instruction::I32GtS,
            0x4B => Instruction::I32GtU,
            0x4C => Instruction::I32LeS,
            0x4D => Instruction::I32LeU,
            0x4E => Instruction::I32GeS,
            0x4F => Instruction::I32GeU,
            0x50 => Instruction::I64Eqz,
            0x51 => Instruction::I64Eq,
            0x52 => Instruction::I64Ne,
            0x53 => Instruction::I64LtS,
            0x54 => Instruction::I64LtU,
            0x55 => Instruction::I64GtS,
            0x56 => Instruction::I64GtU,
            0x57 => Instruction::I64LeS,
            0x58 => Instruction::I64LeU,
            0x59 => Instruction::I64GeS,
            0x5A => Instruction::I64GeU,
            0x5B => Instruction::F32Eq,
            0x5C => Instruction::F32Ne,
            0x5D => Instruction::F32Lt,
            0x5E => Instruction::F32Gt,
            0x5F => Instruction::F32Le,
            0x60 => Instruction::F32Ge,
            0x61 => Instruction::F64Eq,
            0x62 => Instruction::F64Ne,
            0x63 => Instruction::F64Lt,
            0x64 => Instruction::F64Gt,
            0x65 => Instruction::F64Le,
            0x66 => Instruction::F64Ge,
            0x67 => Instruction::I32Clz,
            0x68 => Instruction::I32Ctz,
            0x69 => Instruction::I32Popcnt,
            0x6A => Instruction::I32Add,
            0x6B => Instruction::I32Sub,
            0x6C => Instruction::I32Mul,
            0x6D => Instruction::I32DivS,
            0x6E => Instruction::I32DivU,
            0x6F => Instruction::I32RemS,
            0x70 => Instruction::I32RemU,
            0x71 => Instruction::I32And,
            0x72 => Instruction::I32Or,
            0x73 => Instruction::I32Xor,
            0x74 => Instruction::I32Shl,
            0x75 => Instruction::I32ShrS,
            0x76 => Instruction::I32ShrU,
            0x77 => Instruction::I32Rotl,
            0x78 => Instruction::I32Rotr,
            0x79 => Instruction::I64Clz,
            0x7A => Instruction::I64Ctz,
            0x7B => Instruction::I64Popcnt,
            0x7C => Instruction::I64Add,
            0x7D => Instruction::I64Sub,
            0x7E => Instruction::I64Mul,
            0x7F => Instruction::I64DivS,
            0x80 => Instruction::I64DivU,
            0x81 => Instruction::I64RemS,
            0x82 => Instruction::I64RemU,
            0x83 => Instruction::I64And,
            0x84 => Instruction::I64Or,
            0x85 => Instruction::I64Xor,
            0x86 => Instruction::I64Shl,
            0x87 => Instruction::I64ShrS,
            0x88 => Instruction::I64ShrU,
            0x89 => Instruction::I64Rotl,
            0x8A => Instruction::I64Rotr,
            0x8B => Instruction::F32Abs,
            0x8C => Instruction::F32Neg,
            0x8D => Instruction::F32Ceil,
            0x8E => Instruction::F32Floor,
            0x8F => Instruction::F32Trunc,
            0x90 => Instruction::F32Nearest,
            0x91 => Instruction::F32Sqrt,
            0x92 => Instruction::F32Add,
            0x93 => Instruction::F32Sub,
            0x94 => Instruction::F32Mul,
            0x95 => Instruction::F32Div,
            0x96 => Instruction::F32Min,
            0x97 => Instruction::F32Max,
            0x98 => Instruction::F32Copysign,
            0x99 => Instruction::F64Abs,
            0x9A => Instruction::F64Neg,
            0x9B => Instruction::F64Ceil,
            0x9C => Instruction::F64Floor,
            0x9D => Instruction::F64Trunc,
            0x9E => Instruction::F64Nearest,
            0x9F => Instruction::F64Sqrt,
            0xA0 => Instruction::F64Add,
            0xA1 => Instruction::F64Sub,
            0xA2 => Instruction::F64Mul,
            0xA3 => Instruction::F64Div,
            0xA4 => Instruction::F64Min,
            0xA5 => Instruction::F64Max,
            0xA6 => Instruction::F64Copysign,
            0xA7 => Instruction::I32WrapI64,
            0xA8 => Instruction::I32TruncF32S,
            0xA9 => Instruction::I32TruncF32U,
            0xAA => Instruction::I32TruncF64S,
            0xAB => Instruction::I32TruncF64U,
            0xAC => Instruction::I64ExtendI32S,
            0xAD => Instruction::I64ExtendI32U,
            0xAE => Instruction::I64TruncF32S,
            0xAF => Instruction::I64TruncF32U,
            0xB0 => Instruction::I64TruncF64S,
            0xB1 => Instruction::I64TruncF64U,
            0xB2 => Instruction::F32ConvertI32S,
            0xB3 => Instruction::F32ConvertI32U,
            0xB4 => Instruction::F32ConvertI64S,
            0xB5 => Instruction::F32ConvertI64U,
            0xB6 => Instruction::F32DemoteF64,
            0xB7 => Instruction::F64ConvertI32S,
            0xB8 => Instruction::F64ConvertI32U,
            0xB9 => Instruction::F64ConvertI64S,
            0xBA => Instruction::F64ConvertI64U,
            0xBB => Instruction::F64PromoteF32,
            0xBC => Instruction::I32ReinterpretF32,
            0xBD => Instruction::I64ReinterpretF64,
            0xBE => Instruction::F32ReinterpretI32,
            0xBF => Instruction::F64ReinterpretI64,
            0xC0 => Instruction::I32Extend8S,
            0xC1 => Instruction::I32Extend16S,
            0xC2 => Instruction::I64Extend8S,
            0xC3 => Instruction::I64Extend16S,
            0xC4 => Instruction::I64Extend32S,

            // 0xFC is shared by Table, Memory and Numeric instructions
            0xFC => match decode_u32(reader).map_err(NumericError::ReadOpcode)? {
                // --- Numeric saturating truncation ---
                0 => Instruction::I32TruncSatF32S,
                1 => Instruction::I32TruncSatF32U,
                2 => Instruction::I32TruncSatF64S,
                3 => Instruction::I32TruncSatF64U,
                4 => Instruction::I64TruncSatF32S,
                5 => Instruction::I64TruncSatF32U,
                6 => Instruction::I64TruncSatF64S,
                7 => Instruction::I64TruncSatF64U,

                // --- Memory ---
                8 => {
                    let y = DataIdx::decode(reader).map_err(MemoryError::DecodeDataIdx)?;
                    let x = MemIdx::decode(reader).map_err(MemoryError::DecodeMemIdx)?;
                    Instruction::MemoryInit(x, y)
                }
                9 => Instruction::DataDrop(
                    DataIdx::decode(reader).map_err(MemoryError::DecodeDataIdx)?,
                ),
                10 => {
                    let x1 = MemIdx::decode(reader).map_err(MemoryError::DecodeMemIdx)?;
                    let x2 = MemIdx::decode(reader).map_err(MemoryError::DecodeMemIdx)?;
                    Instruction::MemoryCopy(x1, x2)
                }
                11 => {
                    let x = MemIdx::decode(reader).map_err(MemoryError::DecodeMemIdx)?;
                    Instruction::MemoryFill(x)
                }

                // --- Table ---
                12 => {
                    let y = ElemIdx::decode(reader).map_err(TableError::ElemIdx)?;
                    let x = TableIdx::decode(reader).map_err(TableError::TableIdx)?;
                    Instruction::TableInit(x, y)
                }
                13 => Instruction::ElemDrop(ElemIdx::decode(reader).map_err(TableError::ElemIdx)?),
                14 => Instruction::TableCopy(
                    TableIdx::decode(reader).map_err(TableError::TableIdx)?,
                    TableIdx::decode(reader).map_err(TableError::TableIdx)?,
                ),
                15 => {
                    Instruction::TableGrow(TableIdx::decode(reader).map_err(TableError::TableIdx)?)
                }
                16 => {
                    Instruction::TableSize(TableIdx::decode(reader).map_err(TableError::TableIdx)?)
                }
                17 => {
                    Instruction::TableFill(TableIdx::decode(reader).map_err(TableError::TableIdx)?)
                }

                // --- Invalid ---
                n => return Err(ParseError::InvalidMarkerByteAfterFC(n)),
            },

            // --- Vector instructions (5.4.8) ---
            0xFD => match decode_u32(reader).map_err(VectorError::ReadOpcode)? {
                op @ (0..=11 | 92 | 93) => {
                    let m = Memarg::decode(reader).map_err(VectorError::Memarg)?;
                    match op {
                        0 => Instruction::V128Load(m),
                        1 => Instruction::V128Load8x8S(m),
                        2 => Instruction::V128Load8x8U(m),
                        3 => Instruction::V128Load16x4S(m),
                        4 => Instruction::V128Load16x4U(m),
                        5 => Instruction::V128Load32x2S(m),
                        6 => Instruction::V128Load32x2U(m),
                        7 => Instruction::V128Load8Splat(m),
                        8 => Instruction::V128Load16Splat(m),
                        9 => Instruction::V128Load32Splat(m),
                        10 => Instruction::V128Load64Splat(m),
                        92 => Instruction::V128Load32Zero(m),
                        93 => Instruction::V128Load64Zero(m),
                        11 => Instruction::V128Store(m),
                        _ => unreachable!(),
                    }
                }
                op @ 84..=91 => {
                    let m = Memarg::decode(reader).map_err(VectorError::Memarg)?;
                    let l = LaneIdx::decode(reader).map_err(VectorError::LaneIdx)?;
                    match op {
                        84 => Instruction::V128Load8Lane(m, l),
                        85 => Instruction::V128Load16Lane(m, l),
                        86 => Instruction::V128Load32Lane(m, l),
                        87 => Instruction::V128Load64Lane(m, l),
                        88 => Instruction::V128Store8Lane(m, l),
                        89 => Instruction::V128Store16Lane(m, l),
                        90 => Instruction::V128Store32Lane(m, l),
                        91 => Instruction::V128Store64Lane(m, l),
                        _ => unreachable!(),
                    }
                }
                12 => {
                    let mut buf = [0u8; 16];
                    reader
                        .read_exact(&mut buf)
                        .map_err(VectorError::ReadImmediateBytes)?;
                    Instruction::V128Const(buf)
                }
                13 => {
                    let mut immediates = [0u8; 16];
                    reader
                        .read_exact(&mut immediates)
                        .map_err(VectorError::ReadImmediateBytes)?;
                    Instruction::I8x16Shuffle(immediates.map(LaneIdx::new))
                }
                op @ 21..=34 => {
                    let l = LaneIdx::decode(reader).map_err(VectorError::LaneIdx)?;
                    match op {
                        21 => Instruction::I8x16ExtractLaneS(l),
                        22 => Instruction::I8x16ExtractLaneU(l),
                        23 => Instruction::I8x16ReplaceLane(l),
                        24 => Instruction::I16x8ExtractLaneS(l),
                        25 => Instruction::I16x8ExtractLaneU(l),
                        26 => Instruction::I16x8ReplaceLane(l),
                        27 => Instruction::I32x4ExtractLane(l),
                        28 => Instruction::I32x4ReplaceLane(l),
                        29 => Instruction::I64x2ExtractLane(l),
                        30 => Instruction::I64x2ReplaceLane(l),
                        31 => Instruction::F32x4ExtractLane(l),
                        32 => Instruction::F32x4ReplaceLane(l),
                        33 => Instruction::F64x2ExtractLane(l),
                        34 => Instruction::F64x2ReplaceLane(l),
                        _ => unreachable!(),
                    }
                }
                14 => Instruction::I8x16Swizzle,
                256 => Instruction::I8x16RelaxedSwizzle,
                15 => Instruction::I8x16Splat,
                16 => Instruction::I16x8Splat,
                17 => Instruction::I32x4Splat,
                18 => Instruction::I64x2Splat,
                19 => Instruction::F32x4Splat,
                20 => Instruction::F64x2Splat,
                35 => Instruction::I8x16Eq,
                36 => Instruction::I8x16Ne,
                37 => Instruction::I8x16LtS,
                38 => Instruction::I8x16LtU,
                39 => Instruction::I8x16GtS,
                40 => Instruction::I8x16GtU,
                41 => Instruction::I8x16LeS,
                42 => Instruction::I8x16LeU,
                43 => Instruction::I8x16GeS,
                44 => Instruction::I8x16GeU,
                45 => Instruction::I16x8Eq,
                46 => Instruction::I16x8Ne,
                47 => Instruction::I16x8LtS,
                48 => Instruction::I16x8LtU,
                49 => Instruction::I16x8GtS,
                50 => Instruction::I16x8GtU,
                51 => Instruction::I16x8LeS,
                52 => Instruction::I16x8LeU,
                53 => Instruction::I16x8GeS,
                54 => Instruction::I16x8GeU,
                55 => Instruction::I32x4Eq,
                56 => Instruction::I32x4Ne,
                57 => Instruction::I32x4LtS,
                58 => Instruction::I32x4LtU,
                59 => Instruction::I32x4GtS,
                60 => Instruction::I32x4GtU,
                61 => Instruction::I32x4LeS,
                62 => Instruction::I32x4LeU,
                63 => Instruction::I32x4GeS,
                64 => Instruction::I32x4GeU,
                65 => Instruction::F32x4Eq,
                66 => Instruction::F32x4Ne,
                67 => Instruction::F32x4Lt,
                68 => Instruction::F32x4Gt,
                69 => Instruction::F32x4Le,
                70 => Instruction::F32x4Ge,
                71 => Instruction::F64x2Eq,
                72 => Instruction::F64x2Ne,
                73 => Instruction::F64x2Lt,
                74 => Instruction::F64x2Gt,
                75 => Instruction::F64x2Le,
                76 => Instruction::F64x2Ge,
                77 => Instruction::V128Not,
                78 => Instruction::V128And,
                79 => Instruction::V128AndNot,
                80 => Instruction::V128Or,
                81 => Instruction::V128Xor,
                82 => Instruction::V128Bitselect,
                265 => Instruction::I8x16RelaxedLaneSelect,
                266 => Instruction::I16x8RelaxedLaneSelect,
                267 => Instruction::I32x4RelaxedLaneSelect,
                268 => Instruction::I64x2RelaxedLaneSelect,
                83 => Instruction::V128AnyTrue,
                96 => Instruction::I8x16Abs,
                97 => Instruction::I8x16Neg,
                98 => Instruction::I8x16Popcnt,
                99 => Instruction::I8x16AllTrue,
                100 => Instruction::I8x16Bitmask,
                101 => Instruction::I8x16NarrowI16x8S,
                102 => Instruction::I8x16NarrowI16x8U,

                107 => Instruction::I8x16Shl,
                108 => Instruction::I8x16ShrS,
                109 => Instruction::I8x16ShrU,
                110 => Instruction::I8x16Add,
                111 => Instruction::I8x16AddSatS,
                112 => Instruction::I8x16AddSatU,
                113 => Instruction::I8x16Sub,
                114 => Instruction::I8x16SubSatS,
                115 => Instruction::I8x16SubSatU,
                118 => Instruction::I8x16MinS,
                119 => Instruction::I8x16MinU,
                120 => Instruction::I8x16MaxS,
                121 => Instruction::I8x16MaxU,
                123 => Instruction::I8x16AvgrU,
                124 => Instruction::I16x8ExtaddPairwiseI8x16S,
                125 => Instruction::I16x8ExtaddPairwiseI8x16U,

                126 => Instruction::I32x4ExtaddPairwiseI16x8S,
                127 => Instruction::I32x4ExtaddPairwiseI16x8U,
                128 => Instruction::I16x8Abs,
                129 => Instruction::I16x8Neg,
                130 => Instruction::I16x8Q15MulrSatS,
                131 => Instruction::I16x8AllTrue,
                132 => Instruction::I16x8Bitmask,
                133 => Instruction::I16x8NarrowI32x4S,
                134 => Instruction::I16x8NarrowI32x4U,
                135 => Instruction::I16x8ExtendLowI8x16S,
                136 => Instruction::I16x8ExtendHighI8x16S,
                137 => Instruction::I16x8ExtendLowI8x16U,
                138 => Instruction::I16x8ExtendHighI8x16U,
                139 => Instruction::I16x8Shl,
                140 => Instruction::I16x8ShrS,
                141 => Instruction::I16x8ShrU,
                142 => Instruction::I16x8Add,
                143 => Instruction::I16x8AddSatS,
                144 => Instruction::I16x8AddSatU,
                145 => Instruction::I16x8Sub,
                146 => Instruction::I16x8SubSatS,
                147 => Instruction::I16x8SubSatU,
                149 => Instruction::I16x8Mul,
                150 => Instruction::I16x8MinS,
                151 => Instruction::I16x8MinU,
                152 => Instruction::I16x8MaxS,
                153 => Instruction::I16x8MaxU,
                155 => Instruction::I16x8AvgrU,
                273 => Instruction::I16x8RelaxedQ15MulrS,
                156 => Instruction::I16x8ExtmulLowI8x16S,
                157 => Instruction::I16x8ExtmulHighI8x16S,
                158 => Instruction::I16x8ExtmulLowI8x16U,
                159 => Instruction::I16x8ExtmulHighI8x16U,
                274 => Instruction::I16x8RelaxedDotSI8x16,
                275 => Instruction::I32x4RelaxedDotAddSI16x8,
                160 => Instruction::I32x4Abs,
                161 => Instruction::I32x4Neg,
                163 => Instruction::I32x4AllTrue,
                164 => Instruction::I32x4Bitmask,
                167 => Instruction::I32x4ExtendLowI16x8S,
                168 => Instruction::I32x4ExtendHighI16x8S,
                169 => Instruction::I32x4ExtendLowI16x8U,
                170 => Instruction::I32x4ExtendHighI16x8U,
                171 => Instruction::I32x4Shl,
                172 => Instruction::I32x4ShrS,
                173 => Instruction::I32x4ShrU,
                174 => Instruction::I32x4Add,
                177 => Instruction::I32x4Sub,
                181 => Instruction::I32x4Mul,
                182 => Instruction::I32x4MinS,
                183 => Instruction::I32x4MinU,
                184 => Instruction::I32x4MaxS,
                185 => Instruction::I32x4MaxU,
                186 => Instruction::I32x4DotI16x8S,
                188 => Instruction::I32x4ExtmulLowI16x8S,
                189 => Instruction::I32x4ExtmulHighI16x8S,
                190 => Instruction::I32x4ExtmulLowI16x8U,
                191 => Instruction::I32x4ExtmulHighI16x8U,
                192 => Instruction::I64x2Abs,
                193 => Instruction::I64x2Neg,
                195 => Instruction::I64x2AllTrue,
                196 => Instruction::I64x2Bitmask,
                199 => Instruction::I64x2ExtendLowI32x4S,
                200 => Instruction::I64x2ExtendHighI32x4S,
                201 => Instruction::I64x2ExtendLowI32x4U,
                202 => Instruction::I64x2ExtendHighI32x4U,
                203 => Instruction::I64x2Shl,
                204 => Instruction::I64x2ShrS,
                205 => Instruction::I64x2ShrU,
                206 => Instruction::I64x2Add,
                209 => Instruction::I64x2Sub,
                213 => Instruction::I64x2Mul,
                214 => Instruction::I64x2Eq,
                215 => Instruction::I64x2Ne,
                216 => Instruction::I64x2LtS,
                217 => Instruction::I64x2GtS,
                218 => Instruction::I64x2LeS,
                219 => Instruction::I64x2GeS,
                220 => Instruction::I64x2ExtmulLowI32x4S,
                221 => Instruction::I64x2ExtmulHighI32x4S,
                222 => Instruction::I64x2ExtmulLowI32x4U,
                223 => Instruction::I64x2ExtmulHighI32x4U,

                103 => Instruction::F32x4Ceil,
                104 => Instruction::F32x4Floor,
                105 => Instruction::F32x4Trunc,
                106 => Instruction::F32x4Nearest,
                224 => Instruction::F32x4Abs,
                225 => Instruction::F32x4Neg,
                227 => Instruction::F32x4Sqrt,
                228 => Instruction::F32x4Add,
                229 => Instruction::F32x4Sub,
                230 => Instruction::F32x4Mul,
                231 => Instruction::F32x4Div,
                232 => Instruction::F32x4Min,
                233 => Instruction::F32x4Max,
                234 => Instruction::F32x4PMin,
                235 => Instruction::F32x4PMax,
                269 => Instruction::F32x4RelaxedMin,
                270 => Instruction::F32x4RelaxedMax,
                271 => Instruction::F64x2RelaxedMin,
                272 => Instruction::F64x2RelaxedMax,
                261 => Instruction::F32x4RelaxedMAdd,
                262 => Instruction::F32x4RelaxedNMAdd,
                263 => Instruction::F64x2RelaxedMAdd,
                264 => Instruction::F64x2RelaxedNMAdd,

                116 => Instruction::F64x2Ceil,
                117 => Instruction::F64x2Floor,
                122 => Instruction::F64x2Trunc,
                148 => Instruction::F64x2Nearest,
                236 => Instruction::F64x2Abs,
                237 => Instruction::F64x2Neg,
                239 => Instruction::F64x2Sqrt,
                240 => Instruction::F64x2Add,
                241 => Instruction::F64x2Sub,
                242 => Instruction::F64x2Mul,
                243 => Instruction::F64x2Div,
                244 => Instruction::F64x2Min,
                245 => Instruction::F64x2Max,
                246 => Instruction::F64x2PMin,
                247 => Instruction::F64x2PMax,
                248 => Instruction::I32x4TruncSatF32x4S,
                249 => Instruction::I32x4TruncSatF32x4U,
                250 => Instruction::F32x4ConvertI32x4S,
                251 => Instruction::F32x4ConvertI32x4U,
                252 => Instruction::I32x4TruncSatF64x2SZero,
                253 => Instruction::I32x4TruncSatF64x2UZero,
                254 => Instruction::F64x2ConvertLowI32x4S,
                255 => Instruction::F64x2ConvertLowI32x4U,
                257 => Instruction::I32x4RelaxedTruncF32x4S,
                258 => Instruction::I32x4RelaxedTruncF32x4U,
                259 => Instruction::I32x4RelaxedTruncF64x2SZero,
                260 => Instruction::I32x4RelaxedTruncF64x2UZero,
                94 => Instruction::F32x4DemoteF64x2Zero,
                95 => Instruction::F64x2PromoteLowF32x4,

                n => return Err(VectorError::InvalidOpcode(n).into()),
            },
            n => return Err(ParseError::InvalidOpcode(n)),
        };

        Ok(ParseResult::Instruction(ins))
    }
}

pub(crate) enum ParseResult {
    Instruction(Instruction),
    Else,
    End,
}

#[derive(Debug, Error)]
pub enum MemargError {
    #[error("Invalid alignment flag {0}; should be less than 2^7")]
    InvalidFlagsBit(u32),

    #[error("failed decoding alignment")]
    Align(DecodeU32Error),

    #[error("failed decoding offset")]
    Offset(DecodeU64Error),

    #[error("failed decoding memory index")]
    MemIdx(#[from] DecodeMemIdxError),
}

impl Memarg {
    fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Memarg, MemargError> {
        let n = decode_u32(reader).map_err(MemargError::Align)?;

        let (mem_idx, align) = if n < 2u32.pow(6) {
            (MemIdx(0), n)
        } else if n < 2u32.pow(7) {
            (MemIdx::decode(reader)?, n - 2u32.pow(6))
        } else {
            return Err(MemargError::InvalidFlagsBit(n));
        };

        let offset = decode_u64(reader).map_err(MemargError::Offset)?;

        Ok(Self {
            mem_idx,
            align,
            offset,
        })
    }
}

#[derive(Debug, Error)]
#[error("failed decoding Lane index")]
pub struct LaneIdxError(#[from] pub io::Error);

impl LaneIdx {
    pub(crate) fn new(value: u8) -> Self {
        Self(value)
    }

    fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, LaneIdxError> {
        Ok(Self::new(read_byte(reader)?))
    }
}

#[derive(Debug, Error)]
pub enum BlockTypeError {
    #[error("failed reading block type marker byte")]
    ReadMarkerByte(io::Error),

    #[error("failed decoding block type index")]
    DecodeIndex(#[from] DecodeS33Error),

    #[error("blocktype Type index negative: {0}")]
    NegativeTypeIndex(i64),
}

impl BlockType {
    fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, BlockTypeError> {
        let b = read_byte(reader).map_err(BlockTypeError::ReadMarkerByte)?;
        if b == 0x40 {
            return Ok(Self::Empty);
        }

        let mut reader = Cursor::new([b]).chain(reader);
        if let Ok(t) = ValType::decode(&mut reader) {
            return Ok(Self::T(t));
        }

        // Unlike any other occurrence, the type index in a block type is encoded as a positive
        // signed integer, so that its signed LEB128 bit pattern cannot collide with the encoding
        // of value types or the special code 0x40, which correspond to the LEB128 encoding of
        // negative integers. To avoid any loss in the range of allowed indices, it is treated as a
        // 33 bit signed integer.
        let mut reader = Cursor::new([b]).chain(reader);
        let x = decode_s33(&mut reader)?;
        if x < 0 {
            return Err(BlockTypeError::NegativeTypeIndex(x));
        }

        let x = u32::try_from(x).unwrap();

        Ok(Self::I(x))
    }
}

#[derive(Debug, Error)]
pub enum CatchError {
    #[error("failed reading catch block marker byte")]
    ReadMarkerByte(#[from] io::Error),

    #[error("invalid marker byte: expected 0x00, 0x01, 0x02 or 0x03; got {0:#04X}")]
    InvalidMarkerByte(u8),

    #[error(transparent)]
    DecodeTagIdx(#[from] DecodeTagIdxError),

    #[error(transparent)]
    DecodeLabelIdx(#[from] DecodeLabelIdxError),
}

impl Catch {
    fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, CatchError> {
        let marker = read_byte(reader)?;

        Ok(match marker {
            0x00 => {
                let x = TagIdx::decode(reader)?;
                let l = LabelIdx::decode(reader)?;
                Catch::Catch(x, l)
            }
            0x01 => {
                let x = TagIdx::decode(reader)?;
                let l = LabelIdx::decode(reader)?;
                Catch::CatchRef(x, l)
            }
            0x02 => Catch::CatchAll(LabelIdx::decode(reader)?),
            0x03 => Catch::CatchAllRef(LabelIdx::decode(reader)?),
            n => return Err(CatchError::InvalidMarkerByte(n)),
        })
    }
}

fn decode_castop<R: Read + ?Sized>(r: &mut R) -> Result<(bool, bool), ControlError> {
    match read_byte(r).map_err(ControlError::ReadCastNullabilityMarker)? {
        0x00 => Ok((false, false)),
        0x01 => Ok((true, false)),
        0x02 => Ok((false, true)),
        0x03 => Ok((true, true)),
        n => Err(ControlError::InvalidCastNullabilityMarker(n)),
    }
}
