use crate::core::indices::TypeIdx;
use crate::core::types::tagtype::TagType;
use crate::decode::indices::DecodeTypeIdxError;
use crate::read_byte;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
#[error("failed decoding Tag type")]
pub enum DecodeTagTypeError {
    #[error(transparent)]
    ReadMarkerByte(#[from] io::Error),

    #[error(
        "unexpected TagType marker byte: expected {expected:#04X}; got {0:#04X}",
        expected = TagType::MARKER_BYTE
    )]
    InvalidMarkerByte(u8),

    DecodeTypeIndex(#[from] DecodeTypeIdxError),
}

impl TagType {
    const MARKER_BYTE: u8 = 0x00;

    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeTagTypeError> {
        let b = read_byte(reader)?;
        if b != Self::MARKER_BYTE {
            return Err(DecodeTagTypeError::InvalidMarkerByte(b));
        }

        Ok(TagType(TypeIdx::decode(reader)?))
    }
}
