use crate::decode::integer::{DecodeU32Error, decode_u32};
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeDataCountSectionError {
    #[error("failed decoding Data Segment count")]
    DecodeDataSegmentCount(#[from] DecodeU32Error),
}

pub(crate) fn decode_datacount_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<u32, DecodeDataCountSectionError> {
    Ok(decode_u32(reader)?)
}
