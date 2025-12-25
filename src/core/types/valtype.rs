use super::numtype::NumType;
use super::reftype::RefType;
use super::vectype::VecType;

/// Value types classify the individual values that WebAssembly code can compute with and
/// the values that a variable accepts. They are either number types, vector types, or
/// reference types.
///
/// <https://www.w3.org/TR/wasm-core-2/#value-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-valtype>
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ValType {
    Num(NumType),
    Vec(VecType),
    Ref(RefType),
}
