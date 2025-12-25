use crate::core::types::reftype::RefType;
use crate::decode::FromMarkerByte;
use crate::read_byte;
use phf::phf_ordered_map;
use std::io::{self, Read};
use thiserror::Error;

// Valid marker bytes for [RefType].
#[expect(non_upper_case_globals)]
static RefType_MARKERS: phf::OrderedMap<u8, RefType> = phf_ordered_map! {
  0x70u8 => RefType::Func,
  0x6Fu8 => RefType::Extern,
};

#[derive(Debug, Error)]
#[error(
    "invalid RefType marker byte - expected one of {markers}; got {0:#04X}",
    markers = RefType::markers_formatted()
)]
pub struct InvalidRefTypeMarkerError(pub u8);

impl From<u8> for InvalidRefTypeMarkerError {
    fn from(b: u8) -> Self {
        Self(b)
    }
}

impl FromMarkerByte for RefType {
    type Error = InvalidRefTypeMarkerError;

    fn markers() -> &'static phf::OrderedMap<u8, Self> {
        &RefType_MARKERS
    }
}

#[derive(Debug, Error)]
pub enum DecodeRefTypeError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    InvalidMarkerByte(#[from] InvalidRefTypeMarkerError),
}

impl RefType {
    pub(crate) fn decode<R: Read + ?Sized>(r: &mut R) -> Result<Self, DecodeRefTypeError> {
        let marker = read_byte(r)?;
        Ok(Self::from_marker(marker)?)
    }
}
