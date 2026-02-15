use crate::core::types::globaltype::{GlobalType, Mut};
use crate::core::types::valtype::ValType;
use crate::decode::FromMarkerByte;
use crate::decode::types::valtype::DecodeValTypeError;
use crate::read_byte;
use phf::phf_ordered_map;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeGlobalTypeError {
    #[error("failed decoding Value type")]
    DecodeValueType(#[from] DecodeValTypeError),

    #[error("failed decoding Mutability")]
    DecodeMutability(std::io::Error),

    #[error(transparent)]
    InvalidMutability(#[from] InvalidMutabilityByteError),
}

impl GlobalType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeGlobalTypeError> {
        let valtype = ValType::decode(reader)?;
        let r#mut =
            Mut::from_marker(read_byte(reader).map_err(DecodeGlobalTypeError::DecodeMutability)?)?;

        Ok(GlobalType(r#mut, valtype))
    }
}

// Valid marker bytes for [Mut].
#[expect(non_upper_case_globals)]
static Mut_MARKERS: phf::OrderedMap<u8, Mut> = phf_ordered_map! {
    0x00u8 => Mut::Const,
    0x01u8 => Mut::Var,
};

#[derive(Debug, Error)]
#[error(
    "invalid Mutability marker byte - expected one of {markers}; got {0:#04X}",
    markers = Mut::markers_formatted()
)]
pub struct InvalidMutabilityByteError(pub u8);

impl From<u8> for InvalidMutabilityByteError {
    fn from(b: u8) -> Self {
        Self(b)
    }
}

impl FromMarkerByte for Mut {
    type Error = InvalidMutabilityByteError;

    fn markers() -> &'static phf::OrderedMap<u8, Self> {
        &Mut_MARKERS
    }
}
