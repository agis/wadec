use crate::Expr;
use crate::decode::helpers::{
    DecodeByteVectorError, DecodeVectorError, ParseExpressionError, decode_byte_vector,
    decode_expr, decode_vector,
};
use crate::decode::integer::{DecodeU32Error, decode_u32};
use crate::indices;
use crate::indices::MemIdx;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeDataSectionError {
    #[error("failed decoding Data section")]
    DecodeVector(#[from] DecodeVectorError<DecodeDataSegmentError>),
}

pub(crate) fn decode_data_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Data>, DecodeDataSectionError> {
    Ok(decode_vector(reader, parse_data)?)
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
pub enum DecodeDataSegmentError {
    #[error("failed decoding bitfield")]
    DecodeBitfield(DecodeU32Error),

    #[error("invalid bitfield: expected 0 (passive), 1 or 2 (active); got {0}")]
    InvalidBitfield(u32),

    #[error("failed decoding offset expression")]
    DecodeOffsetExpr(ParseExpressionError),

    #[error("failed decoding init byte vector")]
    DecodeInitVector(#[from] DecodeByteVectorError),

    #[error("failed decoding Memory index")]
    DecodeMemIdx(#[from] indices::MemIdxError),
}

fn parse_data<R: Read + ?Sized>(reader: &mut R) -> Result<Data, DecodeDataSegmentError> {
    let init: Vec<u8>;
    let mode: DataMode;

    (init, mode) = match decode_u32(reader).map_err(DecodeDataSegmentError::DecodeBitfield)? {
        0 => {
            let e = decode_expr(reader).map_err(DecodeDataSegmentError::DecodeOffsetExpr)?;
            (
                decode_byte_vector(reader)?,
                DataMode::Active {
                    memory: MemIdx(0),
                    offset: e,
                },
            )
        }
        1 => (decode_byte_vector(reader)?, DataMode::Passive),
        2 => {
            let x = indices::MemIdx::decode(reader)?;
            let e = decode_expr(reader).map_err(DecodeDataSegmentError::DecodeOffsetExpr)?;

            (
                decode_byte_vector(reader)?,
                DataMode::Active {
                    memory: x,
                    offset: e,
                },
            )
        }
        n => return Err(DecodeDataSegmentError::InvalidBitfield(n)),
    };

    Ok(Data { init, mode })
}
