use crate::core::types::resulttype::ResultType;
use crate::core::types::valtype::ValType;
use crate::decode::helpers::{DecodeListError, decode_list};
use crate::decode::types::valtype::DecodeValTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeResultTypeError {
    #[error(transparent)]
    DecodeList(#[from] DecodeListError<DecodeValTypeError>),
}

pub(super) fn decode_result_type<R: Read + ?Sized>(
    r: &mut R,
) -> Result<ResultType, DecodeResultTypeError> {
    Ok(decode_list(r, ValType::decode)?)
}
