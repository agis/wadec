use crate::core::types::heaptype::*;
use crate::decode::integer::{decode_s33, DecodeS33Error};
use crate::decode::FromMarkerByte;
use crate::read_byte;
use phf::phf_ordered_map;
use std::io::Cursor;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeHeapTypeError {
    #[error("failed reading Heap type marker byte")]
    ReadMarkerByte(#[from] io::Error),

    #[error("failed decoding type index")]
    DecodeTypeIndex(#[from] DecodeS33Error),

    #[error("type index negative: {0}")]
    NegativeTypeIndex(i64),
}

impl HeapType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeHeapTypeError> {
        let b = read_byte(reader)?;

        if let Ok(t) = AbsHeapType::from_marker(b) {
            return Ok(Self::Ht(t));
        }

        let mut chain = Cursor::new([b]).chain(reader);
        let x = decode_s33(&mut chain)?;
        if x < 0 {
            return Err(DecodeHeapTypeError::NegativeTypeIndex(x));
        }

        let x = u32::try_from(x).unwrap();

        Ok(Self::TypeIdx(x))
    }
}

// --------------------------------
// Abstract Heap Type
// -------------------------------
#[expect(non_upper_case_globals)]
static AbsHeapType_MARKERS: phf::OrderedMap<u8, AbsHeapType> = phf_ordered_map! {
    0x69u8 => AbsHeapType::Exn,
    0x6Au8 => AbsHeapType::Array,
    0x6Bu8 => AbsHeapType::Struct,
    0x6Cu8 => AbsHeapType::I31,
    0x6Du8 => AbsHeapType::Eq,
    0x6Eu8 => AbsHeapType::Any,
    0x6Fu8 => AbsHeapType::Extern,
    0x70u8 => AbsHeapType::Func,
    0x71u8 => AbsHeapType::None,
    0x72u8 => AbsHeapType::NoExtern,
    0x73u8 => AbsHeapType::NoFunc,
    0x74u8 => AbsHeapType::NoExn,
};

#[derive(Debug, Error)]
#[error(
    "invalid AbsHeapType marker byte - expected one of {markers}; got {0:#04X}",
    markers = AbsHeapType::markers_formatted()
)]
pub struct InvalidAbsHeapTypeMarkerError(pub u8);

impl From<u8> for InvalidAbsHeapTypeMarkerError {
    fn from(b: u8) -> Self {
        Self(b)
    }
}

impl FromMarkerByte for AbsHeapType {
    type Error = InvalidAbsHeapTypeMarkerError;

    fn markers() -> &'static phf::OrderedMap<u8, Self> {
        &AbsHeapType_MARKERS
    }
}

#[derive(Debug, Error)]
pub enum DecodeAbsHeapTypeError {
    #[error("failed reading Heap type marker byte")]
    ReadMarkerByte(#[from] io::Error),

    #[error(transparent)]
    InvalidMarkerByte(#[from] InvalidAbsHeapTypeMarkerError),
}

impl AbsHeapType {
    pub(crate) fn decode<R: Read + ?Sized>(r: &mut R) -> Result<Self, DecodeAbsHeapTypeError> {
        let marker = read_byte(r)?;
        Ok(Self::from_marker(marker)?)
    }
}
