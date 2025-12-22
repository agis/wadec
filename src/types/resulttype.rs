use crate::types::valtype::ValType;

/// Result types classify the result of executing instructions or functions,
/// which is a sequence of values, written with brackets.
///
/// <https://www.w3.org/TR/wasm-core-2/#result-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-resulttype>
pub type ResultType = Vec<ValType>;
