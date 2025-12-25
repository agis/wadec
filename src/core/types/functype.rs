use super::ValType;

/// Function types classify the signature of functions, mapping a vector of parameters to a vector
/// of results. They are also used to classify the inputs and outputs of instructions.
///
/// <https://www.w3.org/TR/wasm-core-2/#function-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-functype>
#[derive(Debug, PartialEq)]
pub struct FuncType {
    pub parameters: Vec<ValType>,
    pub results: Vec<ValType>,
}
