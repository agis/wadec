use crate::core::types::memtype::MemType;
use crate::decode::helpers::{DecodeVectorError, decode_vector};
use crate::decode::types::{DecodeMemoryTypeError, memtype::parse_memtype};
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeMemorySectionError {
    #[error("failed decoding Memory section")]
    DecodeVector(#[from] DecodeVectorError<DecodeMemoryTypeError>),
}

pub(crate) fn decode_memory_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<MemType>, DecodeMemorySectionError> {
    Ok(decode_vector(reader, parse_memtype)?)
}
