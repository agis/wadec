use crate::core::indices::MemIdx;
use crate::core::{Data, DataMode};
use crate::decode::helpers::{
    DecodeByteVectorError, DecodeListError, ParseExpressionError, decode_byte_vector, decode_expr,
    decode_list,
};
use crate::decode::indices::DecodeMemIdxError;
use crate::decode::integer::{DecodeU32Error, decode_u32};
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeDataSectionError {
    #[error("failed decoding Data section")]
    DecodeVector(#[from] DecodeListError<DecodeDataSegmentError>),
}

pub(crate) fn decode_data_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Data>, DecodeDataSectionError> {
    Ok(decode_list(reader, parse_data)?)
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
    DecodeMemIdx(#[from] DecodeMemIdxError),
}

fn parse_data<R: Read + ?Sized>(reader: &mut R) -> Result<Data, DecodeDataSegmentError> {
    let init: Vec<u8>;
    let mode: DataMode;

    (init, mode) = match decode_u32(reader).map_err(DecodeDataSegmentError::DecodeBitfield)? {
        0 => {
            let (e, _) = decode_expr(reader).map_err(DecodeDataSegmentError::DecodeOffsetExpr)?;
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
            let x = MemIdx::decode(reader)?;
            let (e, _) = decode_expr(reader).map_err(DecodeDataSegmentError::DecodeOffsetExpr)?;

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
