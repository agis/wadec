use crate::decode::helpers::{DecodeVectorError, decode_vector};
use crate::decode::types::DecodeFuncTypeError;
use crate::types::functype::FuncType;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTypeSectionError {
    #[error("failed decoding Type section")]
    DecodeVector(#[from] DecodeVectorError<DecodeFuncTypeError>),
}

pub(crate) fn decode_type_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<FuncType>, DecodeTypeSectionError> {
    Ok(decode_vector(reader, FuncType::decode)?)
}
