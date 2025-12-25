use crate::core::types::numtype::NumType;
use crate::core::types::reftype::RefType;
use crate::core::types::valtype::ValType;
use crate::core::types::vectype::VecType;
use crate::decode::FromMarkerByte;
use crate::read_byte;
use phf::phf_ordered_map;
use std::io::{self, Read};
use thiserror::Error;

// Valid marker bytes for [ValType].
#[expect(non_upper_case_globals)]
static ValType_MARKERS: phf::OrderedMap<u8, ValType> = phf_ordered_map! {
    0x7Fu8 => ValType::Num(NumType::Int32),
    0x7Eu8 => ValType::Num(NumType::Int64),
    0x7Du8 => ValType::Num(NumType::Float32),
    0x7Cu8 => ValType::Num(NumType::Float64),
    0x7Bu8 => ValType::Vec(VecType::V128),
    0x70u8 => ValType::Ref(RefType::Func),
    0x6Fu8 => ValType::Ref(RefType::Extern),
};

#[derive(Debug, Error)]
#[error(
    "invalid ValType marker byte - expected one of {markers}; got {0:#04X}",
    markers = ValType::markers_formatted()
)]
pub struct InvalidValTypeMarkerError(pub u8);

impl From<u8> for InvalidValTypeMarkerError {
    fn from(b: u8) -> Self {
        Self(b)
    }
}

#[derive(Debug, Error)]
pub enum DecodeValTypeError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    InvalidMarkerByte(#[from] InvalidValTypeMarkerError),
}

impl ValType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeValTypeError> {
        Ok(Self::from_marker(read_byte(reader)?)?)
    }
}

impl FromMarkerByte for ValType {
    type Error = InvalidValTypeMarkerError;

    fn markers() -> &'static phf::OrderedMap<u8, Self> {
        &ValType_MARKERS
    }
}
