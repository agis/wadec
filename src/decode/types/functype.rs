use crate::core::types::functype::FuncType;
use crate::decode::types::resulttype::{decode_result_type, DecodeResultTypeError};
use crate::read_byte;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeFuncTypeError {
    #[error(transparent)]
    ReadMarkerByte(#[from] io::Error),

    #[error(
        "unexpected FuncType marker byte: expected {expected:#04X}; got {0:#04X}",
        expected = FuncType::MARKER_BYTE
    )]
    InvalidMarkerByte(u8),

    #[error("failed decoding Parameters")]
    DecodeParameterTypes(#[source] DecodeResultTypeError),

    #[error("failed decoding Results")]
    DecodeResultTypes(#[source] DecodeResultTypeError),
}

#[expect(dead_code)]
impl FuncType {
    const MARKER_BYTE: u8 = 0x60;

    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeFuncTypeError> {
        let b = read_byte(reader)?;
        if b != Self::MARKER_BYTE {
            return Err(DecodeFuncTypeError::InvalidMarkerByte(b));
        }

        let parameters =
            decode_result_type(reader).map_err(DecodeFuncTypeError::DecodeParameterTypes)?;
        let results = decode_result_type(reader).map_err(DecodeFuncTypeError::DecodeResultTypes)?;

        Ok(FuncType {
            parameters,
            results,
        })
    }
}
