use crate::core::Global;
use crate::core::types::GlobalType;
use crate::decode::helpers::{DecodeListError, ParseExpressionError, decode_expr, decode_list};
use crate::decode::types::DecodeGlobalTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeGlobalSectionError {
    #[error("failed decoding Global section")]
    DecodeList(#[from] DecodeListError<DecodeGlobalError>),
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
    let r#type = GlobalType::decode(reader)?;
    let (init, _) = decode_expr(reader)?;

    Ok(Global { r#type, init })
}
