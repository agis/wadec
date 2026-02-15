use crate::core::types::tagtype::TagType;
use crate::decode::helpers::{DecodeListError, decode_list};
use crate::decode::types::tagtype::DecodeTagTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTagSectionError {
    #[error("failed decoding Tag section")]
    DecodeList(#[from] DecodeListError<DecodeTagTypeError>),
}

pub(crate) fn decode_tag_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<TagType>, DecodeTagSectionError> {
    Ok(decode_list(reader, TagType::decode)?)
}
