use crate::core::types::limits::Limits;
use crate::decode::integer::{DecodeU32Error, decode_u32};
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseLimitsError {
    #[error("failed determining presence of max limit")]
    DetermineMaxLimitPresence(std::io::Error),

    #[error("unexpected limits byte: expected 0x00 (false) or 0x01 (true); got {0:#04X}")]
    UnexpectedMaxLimitByte(u8),

    #[error("failed reading minimum limit")]
    ReadMinLimit(DecodeU32Error),

    #[error("failed reading maximum limit")]
    ReadMaxLimit(DecodeU32Error),
}

pub(super) fn parse_limits<R: Read + ?Sized>(reader: &mut R) -> Result<Limits, ParseLimitsError> {
    let has_max =
        match crate::read_byte(reader).map_err(ParseLimitsError::DetermineMaxLimitPresence)? {
            0x00 => false,
            0x01 => true,
            n => return Err(ParseLimitsError::UnexpectedMaxLimitByte(n)),
        };

    let min = decode_u32(reader).map_err(ParseLimitsError::ReadMinLimit)?;
    let mut max = None;

    if has_max {
        max = Some(decode_u32(reader).map_err(ParseLimitsError::ReadMaxLimit)?);
    }

    Ok(Limits { min, max })
}
