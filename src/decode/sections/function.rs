use crate::Expr;
use crate::core::indices::TypeIdx;
use crate::core::types::valtype::ValType;
use crate::decode::helpers::{DecodeVectorError, decode_vector};
use crate::decode::indices::DecodeTypeIdxError;
use std::io::Read;
use thiserror::Error;

/// The funcs component of a module defines a vector of functions.
///
/// Functions are referenced through function indices, starting with the
/// smallest index not referencing a function import.
///
/// <https://www.w3.org/TR/wasm-core-2/#functions>
/// <https://www.w3.org/TR/wasm-core-2/#code-section>
#[derive(Debug, PartialEq)]
pub struct Func {
    /// The type of a function declares its signature by reference to a type defined
    /// in the module. The parameters of the function are referenced through 0-based local indices
    /// in the function's body; they are mutable.
    pub r#type: TypeIdx,

    /// The locals declare a vector of mutable local variables and their types.
    /// These variables are referenced through local indices in the function's
    /// body. The index of the first local is the smallest index not referencing a
    /// parameter.
    pub locals: Vec<ValType>,

    /// The body is an instruction sequence that upon termination must produce an
    /// stack matching the function type's result type.
    pub body: Expr,
}

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
