use crate::core::types::functype::FuncType;
use crate::decode::helpers::{decode_list, DecodeListError};
use crate::decode::types::DecodeFuncTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTypeSectionError {
    #[error("failed decoding Type section")]
    DecodeVector(#[from] DecodeListError<DecodeFuncTypeError>),
}

pub(crate) fn decode_type_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<FuncType>, DecodeTypeSectionError> {
    Ok(decode_list(reader, FuncType::decode)?)
}
