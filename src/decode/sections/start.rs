use crate::indices;
use crate::indices::FuncIdx;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("failed decoding Start section")]
pub struct DecodeStartSectionError(#[from] pub indices::FuncIdxError);

pub(crate) fn decode_start_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<FuncIdx, DecodeStartSectionError> {
    Ok(FuncIdx::decode(reader)?)
}
