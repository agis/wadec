use crate::core::types::tabletype::TableType;
use crate::decode::helpers::{DecodeListError, decode_list};
use crate::decode::types::DecodeTableTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTableSectionError {
    #[error("failed decoding Table section")]
    DecodeVector(#[from] DecodeListError<DecodeTableTypeError>),
}

pub(crate) fn decode_table_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<TableType>, DecodeTableSectionError> {
    Ok(decode_list(reader, TableType::decode)?)
}
