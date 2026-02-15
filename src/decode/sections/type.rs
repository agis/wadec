use crate::core::types::rectype::RecType;
use crate::decode::helpers::{DecodeListError, decode_list};
use crate::decode::types::DecodeRecTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTypeSectionError {
    #[error("failed decoding Type section")]
    DecodeList(#[from] DecodeListError<DecodeRecTypeError>),
}

pub(crate) fn decode_type_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<RecType>, DecodeTypeSectionError> {
    Ok(decode_list(reader, RecType::decode)?)
}
