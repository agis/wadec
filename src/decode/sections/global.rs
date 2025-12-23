use crate::Expr;
use crate::decode::helpers::{DecodeVectorError, ParseExpressionError, decode_expr, decode_vector};
use crate::decode::types::DecodeGlobalTypeError;
use crate::types::globaltype::GlobalType;
use std::io::Read;
use thiserror::Error;

/// The globals component of a module defines a vector of global variables (or globals for
/// short): Each global stores a single value of the given global type. Its type also
/// specifies whether a global is immutable or mutable. Moreover, each global is initialized
/// with an init value given by a constant initializer expression. Globals are referenced
/// through global indices, starting with the smallest index not referencing a global import.
///
/// <https://www.w3.org/TR/wasm-core-2/#globals>
/// <https://www.w3.org/TR/wasm-core-2/#global-section>
#[derive(Debug, PartialEq)]
pub struct Global {
    pub r#type: GlobalType,
    pub init: Expr,
}

#[derive(Debug, Error)]
pub enum DecodeGlobalSectionError {
    #[error("failed decoding Global section")]
    DecodeVector(#[from] DecodeVectorError<DecodeGlobalError>),
}

pub(crate) fn decode_global_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Global>, DecodeGlobalSectionError> {
    Ok(decode_vector(reader, parse_global)?)
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
