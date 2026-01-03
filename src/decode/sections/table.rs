use crate::core::types::tabletype::TableType;
use crate::decode::helpers::{decode_list, DecodeListError};
use crate::decode::types::DecodeTableError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTableSectionError {
    #[error("failed decoding Table section")]
    DecodeVector(#[from] DecodeListError<DecodeTableError>),
}

pub(crate) fn decode_table_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<TableType>, DecodeTableSectionError> {
    Ok(decode_list(reader, TableType::decode)?)
}
