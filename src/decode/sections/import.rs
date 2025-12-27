use crate::core::indices::TypeIdx;
use crate::core::types::globaltype::GlobalType;
use crate::core::types::tabletype::TableType;
use crate::core::{Import, ImportDesc};
use crate::decode::helpers::{DecodeNameError, DecodeVectorError, decode_name, decode_vector};
use crate::decode::indices::DecodeTypeIdxError;
use crate::decode::types::memtype::parse_memtype;
use crate::decode::types::{DecodeGlobalTypeError, DecodeMemoryTypeError, DecodeTableError};
use std::io;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeImportSectionError {
    #[error("failed decoding Import section")]
    DecodeVector(#[from] DecodeVectorError<DecodeImportError>),
}

pub(crate) fn decode_import_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Import>, DecodeImportSectionError> {
    Ok(decode_vector(reader, parse_import)?)
}

#[derive(Debug, Error)]
pub enum DecodeImportError {
    #[error("failed decoding module name")]
    DecodeModuleName(DecodeNameError),

    #[error("failed decoding entity name")]
    DecodeName(DecodeNameError),

    #[error("failed reading Import descriptor marker byte")]
    ReadDescriptorMarkerByte(io::Error),

    #[error(transparent)]
    DecodeTypeIdx(#[from] DecodeTypeIdxError),

    #[error(transparent)]
    DecodeTable(#[from] DecodeTableError),

    #[error(transparent)]
    DecodeMemType(#[from] DecodeMemoryTypeError),

    #[error(transparent)]
    DecodeGlobalType(#[from] DecodeGlobalTypeError),

    #[error(
        "invalid ImportDesc marker byte: expected 0x00 (type), 0x01 (table), 0x02 (mem) or 0x03 (global); got {0:#04X}"
    )]
    InvalidDescriptorMarkerByte(u8),
}

fn parse_import<R: Read + ?Sized>(reader: &mut R) -> Result<Import, DecodeImportError> {
    let module = decode_name(reader).map_err(DecodeImportError::DecodeModuleName)?;
    let name = decode_name(reader).map_err(DecodeImportError::DecodeName)?;

    // parse desc
    let mut desc_kind = [0u8];
    reader
        .read_exact(&mut desc_kind)
        .map_err(DecodeImportError::ReadDescriptorMarkerByte)?;

    let desc = match desc_kind[0] {
        0x00 => ImportDesc::Type(TypeIdx::decode(reader)?),
        0x01 => ImportDesc::Table(TableType::decode(reader)?),
        0x02 => ImportDesc::Mem(parse_memtype(reader)?),
        0x03 => ImportDesc::Global(GlobalType::decode(reader)?),
        n => return Err(DecodeImportError::InvalidDescriptorMarkerByte(n)),
    };

    Ok(Import { module, name, desc })
}
