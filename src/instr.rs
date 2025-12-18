use crate::index::*;
use crate::integer::{self, *};
use crate::{
    DecodeFloat32Error, DecodeFloat64Error, DecodeRefTypeError, DecodeValTypeError,
    DecodeVectorError, FromMarkerByte, RefType, ValType, parse_f32, parse_f64, parse_vector,
    read_byte,
};
use std::io::{self, Cursor, Read};
use thiserror::Error;

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

    #[error("failed decoding branch table labels")]
    LabelIdxVector(#[from] integer::DecodeU32Error),

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

    #[error("unexpected opcode: {0:#04X}")]
    InvalidOpcode(u8),

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
    ReadOpcode(integer::DecodeU32Error),

    #[error(transparent)]
    DecodeI32(integer::DecodeI32Error),

    #[error(transparent)]
    DecodeI64(integer::DecodeI64Error),

    #[error(transparent)]
    DecodeF32(DecodeFloat32Error),

    #[error(transparent)]
    DecodeF64(DecodeFloat64Error),
}

#[derive(Debug, Error)]
pub enum VectorError {
    #[error("failed reading Vector opcode")]
    ReadOpcode(integer::DecodeU32Error),

    #[error(transparent)]
    Memarg(MemargError),

    #[error(transparent)]
    LaneIdx(LaneIdxError),

    #[error("failed reading immediate bytes")]
    ReadImmediateBytes(io::Error),

    #[error("unexpected Vector opcode: {0:#04X}")]
    InvalidOpcode(u32),
}

impl Instr {
    pub fn parse<R: Read + ?Sized>(reader: &mut R) -> Result<Parsed, ParseError> {
        let mut opcode = [0u8];
        reader.read_exact(&mut opcode)?;

        let ins = match opcode[0] {
            0x0B => return Ok(Parsed::End),
            0x05 => return Ok(Parsed::Else),

            // --- Control instructions (5.4.1) ---
            0x00 => Instr::Unreachable,
            0x01 => Instr::Nop,
            op @ 0x02..=0x04 => {
                let bt = BlockType::read(reader).map_err(ControlError::BlockType)?;
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
                                    _ => return Err(ControlError::UnexpectedElse.into()),
                                }
                            }
                            Instr::If(bt, in1, Some(in2))
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            0x0C => Instr::Br(LabelIdx::read(reader).map_err(ControlError::LabelIdx)?),
            0x0D => Instr::BrIf(LabelIdx::read(reader).map_err(ControlError::LabelIdx)?),
            0x0E => {
                let l = parse_vector(reader, LabelIdx::read)
                    .map_err(ControlError::DecodeLabelIdxVector)?;
                let ln = LabelIdx::read(reader).map_err(ControlError::LabelIdx)?;
                Instr::BrTable(l, ln)
            }
            0x0F => Instr::Return,
            0x10 => Instr::Call(FuncIdx::read(reader).map_err(ControlError::FuncIdx)?),
            0x11 => {
                let y = TypeIdx::read(reader).map_err(ControlError::TypeIdx)?;
                let x = TableIdx::read(reader).map_err(ControlError::TableIdx)?;
                Instr::CallIndirect(x, y)
            }

            // --- Reference instructions (5.4.2) ---
            0xD0 => Instr::RefNull(RefType::read(reader).map_err(ReferenceError::RefType)?),
            0xD1 => Instr::RefIsNull,
            0xD2 => Instr::RefFunc(FuncIdx::read(reader).map_err(ReferenceError::FuncIdx)?),

            // --- Parametric instructions (5.4.3) ---
            0x1A => Instr::Drop,
            0x1B => Instr::Select(None),
            0x1C => Instr::Select(Some(
                parse_vector(reader, ValType::read).map_err(ParametricError::DecodeVector)?,
            )),

            // --- Variable instructions (5.4.4) ---
            0x20 => Instr::LocalGet(LocalIdx::read(reader).map_err(VariableError::LocalIdx)?),
            0x21 => Instr::LocalSet(LocalIdx::read(reader).map_err(VariableError::LocalIdx)?),
            0x22 => Instr::LocalTee(LocalIdx::read(reader).map_err(VariableError::LocalIdx)?),
            0x23 => Instr::GlobalGet(GlobalIdx::read(reader).map_err(VariableError::GlobalIdx)?),
            0x24 => Instr::GlobalSet(GlobalIdx::read(reader).map_err(VariableError::GlobalIdx)?),

            // --- Table instructions (5.4.5) ---
            0x25 => Instr::TableGet(TableIdx::read(reader).map_err(TableError::TableIdx)?),
            0x26 => Instr::TableSet(TableIdx::read(reader).map_err(TableError::TableIdx)?),

            // --- Memory instructions (5.4.6) ---
            op @ 0x28..=0x3E => {
                let m = Memarg::read(reader).map_err(MemoryError::DecodeMemarg)?;
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
                    n => return Err(MemoryError::InvalidOpcode(n).into()),
                }
            }
            0x3F => {
                let b = read_byte(reader).map_err(MemoryError::ReadReservedByte)?;
                if b != 0x00 {
                    return Err(MemoryError::InvalidMemorySizeByte(b))?;
                }
                Instr::MemorySize
            }
            0x40 => {
                let b = read_byte(reader).map_err(MemoryError::ReadReservedByte)?;
                if b != 0x00 {
                    return Err(MemoryError::InvalidMemoryGrowByte(b))?;
                }
                Instr::MemoryGrow
            }

            // --- Numeric instructions (5.4.7) ---
            0x41 => Instr::I32Const(read_i32(reader).map_err(NumericError::DecodeI32)?),
            0x42 => Instr::I64Const(read_i64(reader).map_err(NumericError::DecodeI64)?),
            0x43 => Instr::F32Const(parse_f32(reader).map_err(NumericError::DecodeF32)?),
            0x44 => Instr::F64Const(parse_f64(reader).map_err(NumericError::DecodeF64)?),
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
            0xFC => match read_u32(reader).map_err(NumericError::ReadOpcode)? {
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
                    let x = DataIdx::read(reader).map_err(MemoryError::DecodeDataIdx)?;
                    let b = read_byte(reader).map_err(MemoryError::ReadReservedByte)?;
                    if b != 0x00 {
                        return Err(MemoryError::InvalidMemoryInitByte(b))?;
                    }
                    Instr::MemoryInit(x)
                }
                9 => Instr::DataDrop(DataIdx::read(reader).map_err(MemoryError::DecodeDataIdx)?),
                10 => {
                    let mut buf = [0u8; 2];
                    reader
                        .read_exact(&mut buf)
                        .map_err(MemoryError::ReadReservedBytes)?;
                    if buf != [0u8, 0u8] {
                        return Err(MemoryError::InvalidMemoryCopyBytes(buf))?;
                    }
                    Instr::MemoryCopy
                }
                11 => {
                    let b = read_byte(reader).map_err(MemoryError::ReadReservedByte)?;
                    if b != 0x00 {
                        return Err(MemoryError::InvalidMemoryFillByte(b))?;
                    }
                    Instr::MemoryFill
                }

                // --- Table ---
                12 => {
                    let y = ElemIdx::read(reader).map_err(TableError::ElemIdx)?;
                    let x = TableIdx::read(reader).map_err(TableError::TableIdx)?;
                    Instr::TableInit(x, y)
                }
                13 => Instr::ElemDrop(ElemIdx::read(reader).map_err(TableError::ElemIdx)?),
                14 => Instr::TableCopy(
                    TableIdx::read(reader).map_err(TableError::TableIdx)?,
                    TableIdx::read(reader).map_err(TableError::TableIdx)?,
                ),
                15 => Instr::TableGrow(TableIdx::read(reader).map_err(TableError::TableIdx)?),
                16 => Instr::TableSize(TableIdx::read(reader).map_err(TableError::TableIdx)?),
                17 => Instr::TableFill(TableIdx::read(reader).map_err(TableError::TableIdx)?),

                // --- Invalid ---
                n => return Err(ParseError::InvalidMarkerByteAfterFC(n)),
            },

            // --- Vector instructions (5.4.8) ---
            0xFD => match read_u32(reader).map_err(VectorError::ReadOpcode)? {
                op @ (0..=11 | 92 | 93) => {
                    let m = Memarg::read(reader).map_err(VectorError::Memarg)?;
                    match op {
                        0 => Instr::V128Load(m),
                        1 => Instr::V128Load8x8S(m),
                        2 => Instr::V128Load8x8U(m),
                        3 => Instr::V128Load16x4S(m),
                        4 => Instr::V128Load16x4U(m),
                        5 => Instr::V128Load32x2S(m),
                        6 => Instr::V128Load32x2U(m),
                        7 => Instr::V128Load8Splat(m),
                        8 => Instr::V128Load16Splat(m),
                        9 => Instr::V128Load32Splat(m),
                        10 => Instr::V128Load64Splat(m),
                        92 => Instr::V128Load32Zero(m),
                        93 => Instr::V128Load64Zero(m),
                        11 => Instr::V128Store(m),
                        _ => unreachable!(),
                    }
                }
                op @ 84..=91 => {
                    let m = Memarg::read(reader).map_err(VectorError::Memarg)?;
                    let l = LaneIdx::read(reader).map_err(VectorError::LaneIdx)?;
                    match op {
                        84 => Instr::V128Load8Lane(m, l),
                        85 => Instr::V128Load16Lane(m, l),
                        86 => Instr::V128Load32Lane(m, l),
                        87 => Instr::V128Load64Lane(m, l),
                        88 => Instr::V128Store8Lane(m, l),
                        89 => Instr::V128Store16Lane(m, l),
                        90 => Instr::V128Store32Lane(m, l),
                        91 => Instr::V128Store64Lane(m, l),
                        _ => unreachable!(),
                    }
                }
                12 => {
                    let mut buf = [0u8; 16];
                    reader
                        .read_exact(&mut buf)
                        .map_err(VectorError::ReadImmediateBytes)?;
                    Instr::V128Const(buf)
                }
                13 => {
                    let mut immediates = [0u8; 16];
                    reader
                        .read_exact(&mut immediates)
                        .map_err(VectorError::ReadImmediateBytes)?;
                    Instr::I8x16Shuffle(immediates.map(LaneIdx))
                }
                op @ 21..=34 => {
                    let l = LaneIdx::read(reader).map_err(VectorError::LaneIdx)?;
                    match op {
                        21 => Instr::I8x16ExtractLaneS(l),
                        22 => Instr::I8x16ExtractLaneU(l),
                        23 => Instr::I8x16ReplaceLane(l),
                        24 => Instr::I16x8ExtractLaneS(l),
                        25 => Instr::I16x8ExtractLaneU(l),
                        26 => Instr::I16x8ReplaceLane(l),
                        27 => Instr::I32x4ExtractLane(l),
                        28 => Instr::I32x4ReplaceLane(l),
                        29 => Instr::I64x2ExtractLane(l),
                        30 => Instr::I64x2ReplaceLane(l),
                        31 => Instr::F32x4ExtractLane(l),
                        32 => Instr::F32x4ReplaceLane(l),
                        33 => Instr::F64x2ExtractLane(l),
                        34 => Instr::F64x2ReplaceLane(l),
                        _ => unreachable!(),
                    }
                }
                14 => Instr::I8x16Swizzle,
                15 => Instr::I8x16Splat,
                16 => Instr::I16x8Splat,
                17 => Instr::I32x4Splat,
                18 => Instr::I64x2Splat,
                19 => Instr::F32x4Splat,
                20 => Instr::F64x2Splat,
                35 => Instr::I8x16Eq,
                36 => Instr::I8x16Ne,
                37 => Instr::I8x16LtS,
                38 => Instr::I8x16LtU,
                39 => Instr::I8x16GtS,
                40 => Instr::I8x16GtU,
                41 => Instr::I8x16LeS,
                42 => Instr::I8x16LeU,
                43 => Instr::I8x16GeS,
                44 => Instr::I8x16GeU,
                45 => Instr::I16x8Eq,
                46 => Instr::I16x8Ne,
                47 => Instr::I16x8LtS,
                48 => Instr::I16x8LtU,
                49 => Instr::I16x8GtS,
                50 => Instr::I16x8GtU,
                51 => Instr::I16x8LeS,
                52 => Instr::I16x8LeU,
                53 => Instr::I16x8GeS,
                54 => Instr::I16x8GeU,
                55 => Instr::I32x4Eq,
                56 => Instr::I32x4Ne,
                57 => Instr::I32x4LtS,
                58 => Instr::I32x4LtU,
                59 => Instr::I32x4GtS,
                60 => Instr::I32x4GtU,
                61 => Instr::I32x4LeS,
                62 => Instr::I32x4LeU,
                63 => Instr::I32x4GeS,
                64 => Instr::I32x4GeU,
                65 => Instr::F32x4Eq,
                66 => Instr::F32x4Ne,
                67 => Instr::F32x4Lt,
                68 => Instr::F32x4Gt,
                69 => Instr::F32x4Le,
                70 => Instr::F32x4Ge,
                71 => Instr::F64x2Eq,
                72 => Instr::F64x2Ne,
                73 => Instr::F64x2Lt,
                74 => Instr::F64x2Gt,
                75 => Instr::F64x2Le,
                76 => Instr::F64x2Ge,
                77 => Instr::V128Not,
                78 => Instr::V128And,
                79 => Instr::V128AndNot,
                80 => Instr::V128Or,
                81 => Instr::V128Xor,
                82 => Instr::V128Bitselect,
                83 => Instr::V128AnyTrue,
                96 => Instr::I8x16Abs,
                97 => Instr::I8x16Neg,
                98 => Instr::I8x16Popcnt,
                99 => Instr::I8x16AllTrue,
                100 => Instr::I8x16Bitmask,
                101 => Instr::I8x16NarrowI16x8S,
                102 => Instr::I8x16NarrowI16x8U,

                107 => Instr::I8x16Shl,
                108 => Instr::I8x16ShrS,
                109 => Instr::I8x16ShrU,
                110 => Instr::I8x16Add,
                111 => Instr::I8x16AddSatS,
                112 => Instr::I8x16AddSatU,
                113 => Instr::I8x16Sub,
                114 => Instr::I8x16SubSatS,
                115 => Instr::I8x16SubSatU,
                118 => Instr::I8x16MinS,
                119 => Instr::I8x16MinU,
                120 => Instr::I8x16MaxS,
                121 => Instr::I8x16MaxU,
                123 => Instr::I8x16AvgrU,
                124 => Instr::I16x8ExtaddPairwiseI8x16S,
                125 => Instr::I16x8ExtaddPairwiseI8x16U,

                126 => Instr::I32x4ExtaddPairwiseI16x8S,
                127 => Instr::I32x4ExtaddPairwiseI16x8U,
                128 => Instr::I16x8Abs,
                129 => Instr::I16x8Neg,
                130 => Instr::I16x8Q15MulrSatS,
                131 => Instr::I16x8AllTrue,
                132 => Instr::I16x8Bitmask,
                133 => Instr::I16x8NarrowI32x4S,
                134 => Instr::I16x8NarrowI32x4U,
                135 => Instr::I16x8ExtendLowI8x16S,
                136 => Instr::I16x8ExtendHighI8x16S,
                137 => Instr::I16x8ExtendLowI8x16U,
                138 => Instr::I16x8ExtendHighI8x16U,
                139 => Instr::I16x8Shl,
                140 => Instr::I16x8ShrS,
                141 => Instr::I16x8ShrU,
                142 => Instr::I16x8Add,
                143 => Instr::I16x8AddSatS,
                144 => Instr::I16x8AddSatU,
                145 => Instr::I16x8Sub,
                146 => Instr::I16x8SubSatS,
                147 => Instr::I16x8SubSatU,
                149 => Instr::I16x8Mul,
                150 => Instr::I16x8MinS,
                151 => Instr::I16x8MinU,
                152 => Instr::I16x8MaxS,
                153 => Instr::I16x8MaxU,
                155 => Instr::I16x8AvgrU,
                156 => Instr::I16x8ExtmulLowI8x16S,
                157 => Instr::I16x8ExtmulHighI8x16S,
                158 => Instr::I16x8ExtmulLowI8x16U,
                159 => Instr::I16x8ExtmulHighI8x16U,
                160 => Instr::I32x4Abs,
                161 => Instr::I32x4Neg,
                163 => Instr::I32x4AllTrue,
                164 => Instr::I32x4Bitmask,
                167 => Instr::I32x4ExtendLowI16x8S,
                168 => Instr::I32x4ExtendHighI16x8S,
                169 => Instr::I32x4ExtendLowI16x8U,
                170 => Instr::I32x4ExtendHighI16x8U,
                171 => Instr::I32x4Shl,
                172 => Instr::I32x4ShrS,
                173 => Instr::I32x4ShrU,
                174 => Instr::I32x4Add,
                177 => Instr::I32x4Sub,
                181 => Instr::I32x4Mul,
                182 => Instr::I32x4MinS,
                183 => Instr::I32x4MinU,
                184 => Instr::I32x4MaxS,
                185 => Instr::I32x4MaxU,
                186 => Instr::I32x4DotI16x8S,
                188 => Instr::I32x4ExtmulLowI16x8S,
                189 => Instr::I32x4ExtmulHighI16x8S,
                190 => Instr::I32x4ExtmulLowI16x8U,
                191 => Instr::I32x4ExtmulHighI16x8U,
                192 => Instr::I64x2Abs,
                193 => Instr::I64x2Neg,
                195 => Instr::I64x2AllTrue,
                196 => Instr::I64x2Bitmask,
                199 => Instr::I64x2ExtendLowI32x4S,
                200 => Instr::I64x2ExtendHighI32x4S,
                201 => Instr::I64x2ExtendLowI32x4U,
                202 => Instr::I64x2ExtendHighI32x4U,
                203 => Instr::I64x2Shl,
                204 => Instr::I64x2ShrS,
                205 => Instr::I64x2ShrU,
                206 => Instr::I64x2Add,
                209 => Instr::I64x2Sub,
                213 => Instr::I64x2Mul,
                214 => Instr::I64x2Eq,
                215 => Instr::I64x2Ne,
                216 => Instr::I64x2LtS,
                217 => Instr::I64x2GtS,
                218 => Instr::I64x2LeS,
                219 => Instr::I64x2GeS,
                220 => Instr::I64x2ExtmulLowI32x4S,
                221 => Instr::I64x2ExtmulHighI32x4S,
                222 => Instr::I64x2ExtmulLowI32x4U,
                223 => Instr::I64x2ExtmulHighI32x4U,

                103 => Instr::F32x4Ceil,
                104 => Instr::F32x4Floor,
                105 => Instr::F32x4Trunc,
                106 => Instr::F32x4Nearest,
                224 => Instr::F32x4Abs,
                225 => Instr::F32x4Neg,
                227 => Instr::F32x4Sqrt,
                228 => Instr::F32x4Add,
                229 => Instr::F32x4Sub,
                230 => Instr::F32x4Mul,
                231 => Instr::F32x4Div,
                232 => Instr::F32x4Min,
                233 => Instr::F32x4Max,
                234 => Instr::F32x4PMin,
                235 => Instr::F32x4PMax,

                116 => Instr::F64x2Ceil,
                117 => Instr::F64x2Floor,
                122 => Instr::F64x2Trunc,
                148 => Instr::F64x2Nearest,
                236 => Instr::F64x2Abs,
                237 => Instr::F64x2Neg,
                239 => Instr::F64x2Sqrt,
                240 => Instr::F64x2Add,
                241 => Instr::F64x2Sub,
                242 => Instr::F64x2Mul,
                243 => Instr::F64x2Div,
                244 => Instr::F64x2Min,
                245 => Instr::F64x2Max,
                246 => Instr::F64x2PMin,
                247 => Instr::F64x2PMax,
                248 => Instr::I32x4TruncSatF32x4S,
                249 => Instr::I32x4TruncSatF32x4U,
                250 => Instr::F32x4ConvertI32x4S,
                251 => Instr::F32x4ConvertI32x4U,
                252 => Instr::I32x4TruncSatF64x2SZero,
                253 => Instr::I32x4TruncSatF64x2UZero,
                254 => Instr::F64x2ConvertLowI32x4S,
                255 => Instr::F64x2ConvertLowI32x4U,
                94 => Instr::F32x4DemoteF64x2Zero,
                95 => Instr::F64x2PromoteLowF32x4,

                n => return Err(VectorError::InvalidOpcode(n).into()),
            },
            n => return Err(ParseError::InvalidOpcode(n)),
        };

        Ok(Parsed::Instr(ins))
    }
}

pub enum Parsed {
    Instr(Instr),
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
    Align(integer::DecodeU32Error),

    #[error("failed decoding offset")]
    Offset(integer::DecodeU32Error),
}

impl Memarg {
    fn read<R: Read + ?Sized>(reader: &mut R) -> Result<Memarg, MemargError> {
        let align = read_u32(reader).map_err(MemargError::Align)?;
        let offset = read_u32(reader).map_err(MemargError::Offset)?;

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
pub struct LaneIdxError(#[from] io::Error);

impl LaneIdx {
    fn read<R: Read + ?Sized>(reader: &mut R) -> Result<Self, LaneIdxError> {
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
    DecodeIndex(#[from] integer::DecodeI64Error),

    #[error("blocktype Type index negative: {0}")]
    NegativeTypeIndex(i64),

    #[error("blocktype Type index too large (max={max}): {0}", max = u32::MAX)]
    TypeIndexTooLarge(i64),
}

impl BlockType {
    fn read<R: Read + ?Sized>(reader: &mut R) -> Result<Self, BlockTypeError> {
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
        let x = read_i64(&mut chain)?;
        if x < 0 {
            return Err(BlockTypeError::NegativeTypeIndex(x));
        }

        let x = u32::try_from(x).map_err(|_| BlockTypeError::TypeIndexTooLarge(x))?;

        Ok(Self::X(x))
    }
}
