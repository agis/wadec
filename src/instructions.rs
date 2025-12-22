//! WebAssembly instructions representation and decoding.
//!
//! Defined in <https://www.w3.org/TR/wasm-core-2/#instructions>
use crate::decode::FromMarkerByte;
use crate::decode::helpers::{DecodeFloat32Error, DecodeFloat64Error, DecodeVectorError};
use crate::decode::helpers::{decode_f32, decode_f64, decode_vector};
use crate::decode::types::{DecodeRefTypeError, DecodeValTypeError};
use crate::indices::*;
use crate::integer::{
    DecodeI32Error, DecodeI64Error, DecodeU32Error, decode_i32, decode_i64, decode_u32,
};
use crate::read_byte;
use crate::types::{reftype::RefType, valtype::ValType};
use std::io::{self, Cursor, Read};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // --- Control instructions (5.4.1) ---
    Unreachable,
    Nop,
    Block(BlockType, Vec<Instruction>),
    Loop(BlockType, Vec<Instruction>),
    If(BlockType, Vec<Instruction>, Option<Vec<Instruction>>),
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

    // --- Vector instructions (5.4.8) ---
    V128Load(Memarg),
    V128Load8x8S(Memarg),
    V128Load8x8U(Memarg),
    V128Load16x4S(Memarg),
    V128Load16x4U(Memarg),
    V128Load32x2S(Memarg),
    V128Load32x2U(Memarg),
    V128Load8Splat(Memarg),
    V128Load16Splat(Memarg),
    V128Load32Splat(Memarg),
    V128Load64Splat(Memarg),
    V128Load32Zero(Memarg),
    V128Load64Zero(Memarg),
    V128Store(Memarg),
    V128Load8Lane(Memarg, LaneIdx),
    V128Load16Lane(Memarg, LaneIdx),
    V128Load32Lane(Memarg, LaneIdx),
    V128Load64Lane(Memarg, LaneIdx),
    V128Store8Lane(Memarg, LaneIdx),
    V128Store16Lane(Memarg, LaneIdx),
    V128Store32Lane(Memarg, LaneIdx),
    V128Store64Lane(Memarg, LaneIdx),
    V128Const([u8; 16]),
    I8x16Shuffle([LaneIdx; 16]),
    I8x16ExtractLaneS(LaneIdx),
    I8x16ExtractLaneU(LaneIdx),
    I8x16ReplaceLane(LaneIdx),
    I16x8ExtractLaneS(LaneIdx),
    I16x8ExtractLaneU(LaneIdx),
    I16x8ReplaceLane(LaneIdx),
    I32x4ExtractLane(LaneIdx),
    I32x4ReplaceLane(LaneIdx),
    I64x2ExtractLane(LaneIdx),
    I64x2ReplaceLane(LaneIdx),
    F32x4ExtractLane(LaneIdx),
    F32x4ReplaceLane(LaneIdx),
    F64x2ExtractLane(LaneIdx),
    F64x2ReplaceLane(LaneIdx),
    I8x16Swizzle,
    I8x16Splat,
    I16x8Splat,
    I32x4Splat,
    I64x2Splat,
    F32x4Splat,
    F64x2Splat,
    I8x16Eq,
    I8x16Ne,
    I8x16LtS,
    I8x16LtU,
    I8x16GtS,
    I8x16GtU,
    I8x16LeS,
    I8x16LeU,
    I8x16GeS,
    I8x16GeU,
    I16x8Eq,
    I16x8Ne,
    I16x8LtS,
    I16x8LtU,
    I16x8GtS,
    I16x8GtU,
    I16x8LeS,
    I16x8LeU,
    I16x8GeS,
    I16x8GeU,
    I32x4Eq,
    I32x4Ne,
    I32x4LtS,
    I32x4LtU,
    I32x4GtS,
    I32x4GtU,
    I32x4LeS,
    I32x4LeU,
    I32x4GeS,
    I32x4GeU,
    I64x2Eq,
    I64x2Ne,
    I64x2LtS,
    I64x2GtS,
    I64x2LeS,
    I64x2GeS,
    F32x4Eq,
    F32x4Ne,
    F32x4Lt,
    F32x4Gt,
    F32x4Le,
    F32x4Ge,
    F64x2Eq,
    F64x2Ne,
    F64x2Lt,
    F64x2Gt,
    F64x2Le,
    F64x2Ge,
    V128Not,
    V128And,
    V128AndNot,
    V128Or,
    V128Xor,
    V128Bitselect,
    V128AnyTrue,
    I8x16Abs,
    I8x16Neg,
    I8x16Popcnt,
    I8x16AllTrue,
    I8x16Bitmask,
    I8x16NarrowI16x8S,
    I8x16NarrowI16x8U,
    I8x16Shl,
    I8x16ShrS,
    I8x16ShrU,
    I8x16Add,
    I8x16AddSatS,
    I8x16AddSatU,
    I8x16Sub,
    I8x16SubSatS,
    I8x16SubSatU,
    I8x16MinS,
    I8x16MinU,
    I8x16MaxS,
    I8x16MaxU,
    I8x16AvgrU,
    I16x8ExtaddPairwiseI8x16S,
    I16x8ExtaddPairwiseI8x16U,
    I16x8Abs,
    I16x8Neg,
    I16x8Q15MulrSatS,
    I16x8AllTrue,
    I16x8Bitmask,
    I16x8NarrowI32x4S,
    I16x8NarrowI32x4U,
    I16x8ExtendLowI8x16S,
    I16x8ExtendHighI8x16S,
    I16x8ExtendLowI8x16U,
    I16x8ExtendHighI8x16U,
    I16x8Shl,
    I16x8ShrS,
    I16x8ShrU,
    I16x8Add,
    I16x8AddSatS,
    I16x8AddSatU,
    I16x8Sub,
    I16x8SubSatS,
    I16x8SubSatU,
    I16x8Mul,
    I16x8MinS,
    I16x8MinU,
    I16x8MaxS,
    I16x8MaxU,
    I16x8AvgrU,
    I16x8ExtmulLowI8x16S,
    I16x8ExtmulHighI8x16S,
    I16x8ExtmulLowI8x16U,
    I16x8ExtmulHighI8x16U,
    I32x4ExtaddPairwiseI16x8S,
    I32x4ExtaddPairwiseI16x8U,
    I32x4Abs,
    I32x4Neg,
    I32x4AllTrue,
    I32x4Bitmask,
    I32x4ExtendLowI16x8S,
    I32x4ExtendHighI16x8S,
    I32x4ExtendLowI16x8U,
    I32x4ExtendHighI16x8U,
    I32x4Shl,
    I32x4ShrS,
    I32x4ShrU,
    I32x4Add,
    I32x4Sub,
    I32x4Mul,
    I32x4MinS,
    I32x4MinU,
    I32x4MaxS,
    I32x4MaxU,
    I32x4DotI16x8S,
    I32x4ExtmulLowI16x8S,
    I32x4ExtmulHighI16x8S,
    I32x4ExtmulLowI16x8U,
    I32x4ExtmulHighI16x8U,
    I64x2Abs,
    I64x2Neg,
    I64x2AllTrue,
    I64x2Bitmask,
    I64x2ExtendLowI32x4S,
    I64x2ExtendHighI32x4S,
    I64x2ExtendLowI32x4U,
    I64x2ExtendHighI32x4U,
    I64x2Shl,
    I64x2ShrS,
    I64x2ShrU,
    I64x2Add,
    I64x2Sub,
    I64x2Mul,
    I64x2ExtmulLowI32x4S,
    I64x2ExtmulHighI32x4S,
    I64x2ExtmulLowI32x4U,
    I64x2ExtmulHighI32x4U,
    F32x4Ceil,
    F32x4Floor,
    F32x4Trunc,
    F32x4Nearest,
    F32x4Abs,
    F32x4Neg,
    F32x4Sqrt,
    F32x4Add,
    F32x4Sub,
    F32x4Mul,
    F32x4Div,
    F32x4Min,
    F32x4Max,
    F32x4PMin,
    F32x4PMax,
    F64x2Ceil,
    F64x2Floor,
    F64x2Trunc,
    F64x2Nearest,
    F64x2Abs,
    F64x2Neg,
    F64x2Sqrt,
    F64x2Add,
    F64x2Sub,
    F64x2Mul,
    F64x2Div,
    F64x2Min,
    F64x2Max,
    F64x2PMin,
    F64x2PMax,
    I32x4TruncSatF32x4S,
    I32x4TruncSatF32x4U,
    F32x4ConvertI32x4S,
    F32x4ConvertI32x4U,
    I32x4TruncSatF64x2SZero,
    I32x4TruncSatF64x2UZero,
    F64x2ConvertLowI32x4S,
    F64x2ConvertLowI32x4U,
    F32x4DemoteF64x2Zero,
    F64x2PromoteLowF32x4,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("failed reading instruction opcode")]
    ReadOpcode(#[from] io::Error),

    #[error("failed decoding control instruction")]
    Control(#[from] ControlError),

    #[error("failed decoding reference instruction")]
    Reference(#[from] ReferenceError),

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
}

#[derive(Debug, Error)]
pub enum ControlError {
    #[error("failed decoding label index")]
    LabelIdx(#[from] LabelIdxError),

    #[error("failed decoding label index")]
    DecodeLabelIdxVector(#[from] DecodeVectorError<LabelIdxError>),

    #[error("failed decoding function index")]
    FuncIdx(FuncIdxError),

    #[error("failed decoding table index")]
    TableIdx(TableIdxError),

    #[error("failed decoding type index")]
    TypeIdx(TypeIdxError),

    #[error("failed decoding block type")]
    BlockType(BlockTypeError),

    #[error("unexpected `Else` token")]
    UnexpectedElse,
}

#[derive(Debug, Error)]
pub enum ReferenceError {
    #[error("failed decoding reference type")]
    RefType(DecodeRefTypeError),

    #[error("failed decoding function index")]
    FuncIdx(FuncIdxError),
}

#[derive(Debug, Error)]
pub enum ParametricError {
    #[error("failed decoding value type vector")]
    DecodeVector(#[from] DecodeVectorError<DecodeValTypeError>),
}

#[derive(Debug, Error)]
pub enum VariableError {
    #[error("failed decoding Local index")]
    LocalIdx(LocalIdxError),

    #[error("failed decoding Global index")]
    GlobalIdx(GlobalIdxError),
}

#[derive(Debug, Error)]
pub enum TableError {
    #[error("failed decoding table index")]
    TableIdx(TableIdxError),

    #[error("failed decoding element index")]
    ElemIdx(ElemIdxError),
}

#[derive(Debug, Error)]
pub enum MemoryError {
    #[error("failed decoding Memarg")]
    DecodeMemarg(MemargError),

    #[error("failed decoding data index")]
    DecodeDataIdx(DataIdxError),

    #[error("failed reading reserved byte")]
    ReadReservedByte(io::Error),

    #[error("failed reading reserved bytes")]
    ReadReservedBytes(io::Error),

    #[error("unexpected byte {0:#04X} for memory.size")]
    InvalidMemorySizeByte(u8),

    #[error("unexpected byte {0:#04X} for memory.grow")]
    InvalidMemoryGrowByte(u8),

    #[error("unexpected byte {0:#04X} for memory.init")]
    InvalidMemoryInitByte(u8),

    #[error("unexpected bytes {0:?} for memory.copy")]
    InvalidMemoryCopyBytes([u8; 2]),

    #[error("unexpected byte {0:#04X} for memory.fill")]
    InvalidMemoryFillByte(u8),
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

            // --- Control instructions (5.4.1) ---
            0x00 => Instruction::Unreachable,
            0x01 => Instruction::Nop,
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
            0x0C => Instruction::Br(LabelIdx::decode(reader).map_err(ControlError::LabelIdx)?),
            0x0D => Instruction::BrIf(LabelIdx::decode(reader).map_err(ControlError::LabelIdx)?),
            0x0E => {
                let l = decode_vector(reader, LabelIdx::decode)
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

            // --- Reference instructions (5decode) ---
            0xD0 => Instruction::RefNull(RefType::decode(reader).map_err(ReferenceError::RefType)?),
            0xD1 => Instruction::RefIsNull,
            0xD2 => Instruction::RefFunc(FuncIdx::decode(reader).map_err(ReferenceError::FuncIdx)?),

            // --- Parametric instructions (5.4.3) ---
            0x1A => Instruction::Drop,
            0x1B => Instruction::Select(None),
            0x1C => Instruction::Select(Some(
                decode_vector(reader, ValType::decode).map_err(ParametricError::DecodeVector)?,
            )),

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
                let b = read_byte(reader).map_err(MemoryError::ReadReservedByte)?;
                if b != 0x00 {
                    return Err(MemoryError::InvalidMemorySizeByte(b))?;
                }
                Instruction::MemorySize
            }
            0x40 => {
                let b = read_byte(reader).map_err(MemoryError::ReadReservedByte)?;
                if b != 0x00 {
                    return Err(MemoryError::InvalidMemoryGrowByte(b))?;
                }
                Instruction::MemoryGrow
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
                    let x = DataIdx::decode(reader).map_err(MemoryError::DecodeDataIdx)?;
                    let b = read_byte(reader).map_err(MemoryError::ReadReservedByte)?;
                    if b != 0x00 {
                        return Err(MemoryError::InvalidMemoryInitByte(b))?;
                    }
                    Instruction::MemoryInit(x)
                }
                9 => Instruction::DataDrop(
                    DataIdx::decode(reader).map_err(MemoryError::DecodeDataIdx)?,
                ),
                10 => {
                    let mut buf = [0u8; 2];
                    reader
                        .read_exact(&mut buf)
                        .map_err(MemoryError::ReadReservedBytes)?;
                    if buf != [0u8, 0u8] {
                        return Err(MemoryError::InvalidMemoryCopyBytes(buf))?;
                    }
                    Instruction::MemoryCopy
                }
                11 => {
                    let b = read_byte(reader).map_err(MemoryError::ReadReservedByte)?;
                    if b != 0x00 {
                        return Err(MemoryError::InvalidMemoryFillByte(b))?;
                    }
                    Instruction::MemoryFill
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
                    Instruction::I8x16Shuffle(immediates.map(LaneIdx))
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
                156 => Instruction::I16x8ExtmulLowI8x16S,
                157 => Instruction::I16x8ExtmulHighI8x16S,
                158 => Instruction::I16x8ExtmulLowI8x16U,
                159 => Instruction::I16x8ExtmulHighI8x16U,
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

/// Memory is accessed with load and store instructions for the different number types. They all
/// take a memory immediate memarg that contains an address offset and the expected alignment
/// (expressed as the exponent of a power of 2).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Memarg {
    pub align: u32,
    pub offset: u32,
}

#[derive(Debug, Error)]
pub enum MemargError {
    #[error("failed decoding alignment")]
    Align(DecodeU32Error),

    #[error("failed decoding offset")]
    Offset(DecodeU32Error),
}

impl Memarg {
    fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Memarg, MemargError> {
        let align = decode_u32(reader).map_err(MemargError::Align)?;
        let offset = decode_u32(reader).map_err(MemargError::Offset)?;

        Ok(Self { align, offset })
    }
}

/// Vector loads can specify a shape that is half the bit width of v128. Each lane is half its
/// usual size, and the sign extension mode sx then specifies how the smaller lane is extended to
/// the larger lane. Alternatively, vector loads can perform a splat, such that only a single lane
/// of the specified storage size is loaded, and the result is duplicated to all lanes
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LaneIdx(u8);

#[derive(Debug, Error)]
#[error("failed decoding Lane index")]
pub struct LaneIdxError(#[from] pub io::Error);

impl LaneIdx {
    fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, LaneIdxError> {
        Ok(Self(read_byte(reader)?))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockType {
    Empty,
    T(ValType),
    X(u32),
}

#[derive(Debug, Error)]
pub enum BlockTypeError {
    #[error("failed reading block type marker byte")]
    ReadMarkerByte(io::Error),

    #[error("failed decoding block type index")]
    DecodeIndex(#[from] DecodeI64Error),

    #[error("blocktype Type index negative: {0}")]
    NegativeTypeIndex(i64),

    #[error("blocktype Type index too large (max={max}): {0}", max = u32::MAX)]
    TypeIndexTooLarge(i64),
}

impl BlockType {
    fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, BlockTypeError> {
        let b = read_byte(reader).map_err(BlockTypeError::ReadMarkerByte)?;
        if b == 0x40 {
            return Ok(Self::Empty);
        }

        if let Ok(t) = ValType::from_marker(b) {
            return Ok(Self::T(t));
        }

        // Unlike any other occurrence, the type index in a block type is encoded as a positive
        // signed integer, so that its signed LEB128 bit pattern cannot collide with the encoding
        // of value types or the special code 0x40, which correspond to the LEB128 encoding of
        // negative integers. To avoid any loss in the range of allowed indices, it is treated as a
        // 33 bit signed integer.
        let mut chain = Cursor::new([b]).chain(reader);
        let x = decode_i64(&mut chain)?;
        if x < 0 {
            return Err(BlockTypeError::NegativeTypeIndex(x));
        }

        let x = u32::try_from(x).map_err(|_| BlockTypeError::TypeIndexTooLarge(x))?;

        Ok(Self::X(x))
    }
}
