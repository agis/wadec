use crate::core::indices::TypeIdx;
use crate::decode::helpers::{DecodeVectorError, decode_vector};
use crate::decode::indices::DecodeTypeIdxError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeFunctionSectionError {
    #[error("failed decoding Function section")]
    DecodeVector(#[from] DecodeVectorError<DecodeTypeIdxError>),
}

pub(crate) fn decode_function_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<TypeIdx>, DecodeFunctionSectionError> {
    Ok(decode_vector(reader, TypeIdx::decode)?)
}
