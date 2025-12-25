use crate::core::indices::FuncIdx;
use crate::decode::indices::DecodeFuncIdxError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("failed decoding Start section")]
pub struct DecodeStartSectionError(#[from] pub DecodeFuncIdxError);

pub(crate) fn decode_start_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<FuncIdx, DecodeStartSectionError> {
    Ok(FuncIdx::decode(reader)?)
}
