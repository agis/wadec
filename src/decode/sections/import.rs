use crate::core::types::ExternType;
use crate::core::Import;
use crate::decode::helpers::{decode_list, decode_name, DecodeListError, DecodeNameError};
use crate::decode::types::DecodeExternTypeError;
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

/*************************/
/*        Import         */
/*************************/

#[derive(Debug, Error)]
pub enum DecodeImportError {
    #[error("failed decoding module name")]
    DecodeModuleName(DecodeNameError),

    #[error("failed decoding item name")]
    DecodeItemName(DecodeNameError),

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
