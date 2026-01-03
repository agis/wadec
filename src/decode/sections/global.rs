use crate::core::types::GlobalType;
use crate::core::Global;
use crate::decode::helpers::{decode_expr, decode_list, DecodeListError, ParseExpressionError};
use crate::decode::types::DecodeGlobalTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeGlobalSectionError {
    #[error("failed decoding Global section")]
    DecodeVector(#[from] DecodeListError<DecodeGlobalError>),
}

pub(crate) fn decode_global_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Global>, DecodeGlobalSectionError> {
    Ok(decode_list(reader, parse_global)?)
}

#[derive(Debug, Error)]
pub enum DecodeGlobalError {
    #[error(transparent)]
    DecodeGlobalType(#[from] DecodeGlobalTypeError),

    #[error("failed decoding Init")]
    DecodeInit(#[from] ParseExpressionError),
}

fn parse_global<R: Read + ?Sized>(reader: &mut R) -> Result<Global, DecodeGlobalError> {
    Ok(Global {
        r#type: GlobalType::decode(reader)?,
        init: decode_expr(reader)?,
    })
}
