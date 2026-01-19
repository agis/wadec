use crate::core::types::comptype::*;
use crate::core::types::globaltype::Mut;
use crate::core::types::valtype::ValType;
use crate::decode::helpers::{decode_list, DecodeListError};
use crate::decode::types::globaltype::InvalidMutabilityByteError;
use crate::decode::types::resulttype::{decode_result_type, DecodeResultTypeError};
use crate::decode::types::valtype::DecodeValTypeError;
use crate::decode::FromMarkerByte;
use crate::read_byte;
use phf::phf_ordered_map;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeCompTypeError {
    #[error(transparent)]
    ReadMarkerByte(io::Error),

    #[error("unexpected CompType marker byte: expected 0x5E, 0x5F or 0x60; got {0:#04X}")]
    InvalidMarkerByte(u8),

    #[error("failed decoding Field type list")]
    DecodeStruct(#[from] DecodeListError<DecodeFieldTypeError>),

    #[error("failed decoding Function type parameters")]
    DecodeFuncParameters(DecodeResultTypeError),

    #[error("failed decoding Function type results")]
    DecodeFuncResults(DecodeResultTypeError),

    #[error(transparent)]
    DecodeFieldType(#[from] DecodeFieldTypeError),

    #[error(transparent)]
    DecodeStorageType(#[from] DecodeStorageTypeError),

    #[error(transparent)]
    DecodePackType(#[from] InvalidPackTypeMarkerError),
}

impl CompType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeCompTypeError> {
        let marker = read_byte(reader).map_err(DecodeCompTypeError::ReadMarkerByte)?;

        match marker {
            0x5E => {
                let ft = decode_fieldtype(reader)?;
                Ok(CompType::Array(ft))
            }
            0x5F => {
                let ft = decode_list(reader, decode_fieldtype)?;
                Ok(CompType::Struct(ft))
            }
            0x60 => {
                let parameters = decode_result_type(reader)
                    .map_err(DecodeCompTypeError::DecodeFuncParameters)?;
                let results =
                    decode_result_type(reader).map_err(DecodeCompTypeError::DecodeFuncResults)?;

                Ok(CompType::Func {
                    parameters,
                    results,
                })
            }
            n => Err(DecodeCompTypeError::InvalidMarkerByte(n)),
        }
    }
}

/*************************/
/*       FieldType       */
/*************************/
#[derive(Debug, Error)]
pub enum DecodeFieldTypeError {
    #[error(transparent)]
    DecodeStorageType(#[from] DecodeStorageTypeError),

    #[error(transparent)]
    ReadMutabilityMarkerByte(io::Error),

    #[error(transparent)]
    DecodeMutability(#[from] InvalidMutabilityByteError),
}

fn decode_fieldtype<R: Read + ?Sized>(reader: &mut R) -> Result<FieldType, DecodeFieldTypeError> {
    let zt = decode_storagetype(reader)?;

    let mutability_marker =
        read_byte(reader).map_err(DecodeFieldTypeError::ReadMutabilityMarkerByte)?;
    let r#mut = Mut::from_marker(mutability_marker)?;

    Ok(FieldType(r#mut, zt))
}

/*************************/
/*      StorageType      */
/*************************/
#[derive(Debug, Error)]
pub enum DecodeStorageTypeError {
    #[error(transparent)]
    ReadMarkerByte(io::Error),

    #[error("failed decoding as either PackType or ValType with marker byte {marker:#04X}")]
    DecodeValTypeOrPackType {
        marker: u8,
        source: DecodeValTypeError,
    },
}

fn decode_storagetype<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<StorageType, DecodeStorageTypeError> {
    let marker = read_byte(reader).map_err(DecodeStorageTypeError::ReadMarkerByte)?;

    if let Ok(pt) = PackType::from_marker(marker) {
        return Ok(StorageType::PackType(pt));
    }

    // it must be a ValType then; rewind the reader to try to decode it as such
    let mut reader = io::Cursor::new([marker]).chain(reader);
    match ValType::decode(&mut reader) {
        Ok(t) => Ok(StorageType::ValType(t)),
        Err(e) => Err(DecodeStorageTypeError::DecodeValTypeOrPackType { marker, source: e }),
    }
}

/*************************/
/*        PackType       */
/*************************/
#[derive(Debug, Error)]
#[error("invalid PackType marker byte - expected one of {markers}; got {0:#04X}", markers=PackType::markers_formatted())]
pub struct InvalidPackTypeMarkerError(u8);

impl From<u8> for InvalidPackTypeMarkerError {
    fn from(b: u8) -> Self {
        Self(b)
    }
}

#[expect(non_upper_case_globals)]
static PackType_MARKERS: phf::OrderedMap<u8, PackType> = phf_ordered_map! {
            0x77u8 => PackType::I16,
            0x78u8 => PackType::I8,
};

impl FromMarkerByte for PackType {
    type Error = InvalidPackTypeMarkerError;

    fn markers() -> &'static phf::OrderedMap<u8, Self> {
        &PackType_MARKERS
    }
}
