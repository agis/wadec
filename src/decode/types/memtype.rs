use crate::core::types::memtype::MemType;
use crate::decode::types::limits::{ParseLimitsError, parse_limits};
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("failed decoding Memory type")]
pub struct DecodeMemoryTypeError(#[from] pub ParseLimitsError);

pub(crate) fn parse_memtype<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<MemType, DecodeMemoryTypeError> {
    let limits = parse_limits(reader)?;
    Ok(MemType { limits })
}
