use crate::core::types::addrtype::AddrType;
use crate::core::types::limits::Limits;
use crate::decode::integer::{decode_u64, DecodeU64Error};
use crate::read_byte;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseLimitsError {
    #[error("failed reading flag byte")]
    ReadFlagByte(io::Error),

    #[error("unexpected Limits flag byte: expected 0x00, 0x01, 0x04 or 0x05; got {0:#04X}")]
    UnexpectedFlagByte(u8),

    #[error("failed reading minimum limit")]
    ReadMinLimit(DecodeU64Error),

    #[error("failed reading maximum limit")]
    ReadMaxLimit(DecodeU64Error),
}

pub(super) fn parse_limits<R: Read + ?Sized>(reader: &mut R) -> Result<Limits, ParseLimitsError> {
    let (has_max, address_type) = match read_byte(reader).map_err(ParseLimitsError::ReadFlagByte)? {
        0x00 => (false, AddrType::I32),
        0x01 => (true, AddrType::I32),
        0x04 => (false, AddrType::I64),
        0x05 => (true, AddrType::I64),
        n => return Err(ParseLimitsError::UnexpectedFlagByte(n)),
    };

    let min = decode_u64(reader).map_err(ParseLimitsError::ReadMinLimit)?;
    let max = if has_max {
        Some(decode_u64(reader).map_err(ParseLimitsError::ReadMaxLimit)?)
    } else {
        None
    };

    Ok(Limits {
        address_type,
        min,
        max,
    })
}
