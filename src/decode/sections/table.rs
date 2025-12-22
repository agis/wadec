use crate::decode::helpers::{DecodeVectorError, decode_vector};
use crate::decode::types::DecodeTableError;
use crate::types::tabletype::TableType;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTableSectionError {
    #[error("failed decoding Table section")]
    DecodeVector(#[from] DecodeVectorError<DecodeTableError>),
}

pub(crate) fn decode_table_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<TableType>, DecodeTableSectionError> {
    Ok(decode_vector(reader, TableType::decode)?)
}
