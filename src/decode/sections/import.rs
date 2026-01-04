use crate::core::Import;
use crate::core::types::ExternType;
use crate::decode::helpers::{DecodeListError, DecodeNameError, decode_list, decode_name};
use crate::decode::indices::DecodeTypeIdxError;
use crate::decode::types::{
    DecodeExternTypeError, DecodeGlobalTypeError, DecodeMemoryTypeError, DecodeTableTypeError,
    DecodeTagTypeError,
};
use std::io;
use std::io::Read;
use thiserror::Error;
#[derive(Debug, Error)]
pub enum DecodeImportSectionError {
    #[error("failed decoding Import section")]
    DecodeVector(#[from] DecodeListError<DecodeImportError>),
}

pub(crate) fn decode_import_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Import>, DecodeImportSectionError> {
    Ok(decode_list(reader, parse_import)?)
}

#[derive(Debug, Error)]
pub enum DecodeImportError {
    #[error("failed decoding module name")]
    DecodeModuleName(DecodeNameError),

    #[error("failed decoding item name")]
    DecodeItemName(DecodeNameError),

    #[error("failed reading Import descriptor marker byte")]
    ReadMarkerByte(io::Error),

    #[error(
        "invalid marker byte: expected 0x00 (type), 0x01 (table), 0x02 (mem), 0x03 (global), or 0x04 (tag); got {0:#04X}"
    )]
    InvalidMarkerByte(u8),

    #[error(transparent)]
    DecodeTypeIdx(#[from] DecodeTypeIdxError),

    #[error(transparent)]
    DecodeTable(#[from] DecodeTableTypeError),

    #[error(transparent)]
    DecodeMemType(#[from] DecodeMemoryTypeError),

    #[error(transparent)]
    DecodeGlobalType(#[from] DecodeGlobalTypeError),

    #[error(transparent)]
    DecodeTagType(#[from] DecodeTagTypeError),

    #[error(transparent)]
    DecodeExternType(#[from] DecodeExternTypeError),
}

fn parse_import<R: Read + ?Sized>(reader: &mut R) -> Result<Import, DecodeImportError> {
    let module_name = decode_name(reader).map_err(DecodeImportError::DecodeModuleName)?;
    let item_name = decode_name(reader).map_err(DecodeImportError::DecodeItemName)?;
    let extern_type = ExternType::decode(reader)?;

    Ok(Import {
        module_name,
        item_name,
        extern_type,
    })
}
