use crate::core::types::memtype::MemType;
use crate::decode::helpers::{decode_list, DecodeListError};
use crate::decode::types::{memtype::parse_memtype, DecodeMemoryTypeError};
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeMemorySectionError {
    #[error("failed decoding Memory section")]
    DecodeVector(#[from] DecodeListError<DecodeMemoryTypeError>),
}

pub(crate) fn decode_memory_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<MemType>, DecodeMemorySectionError> {
    Ok(decode_list(reader, parse_memtype)?)
}
