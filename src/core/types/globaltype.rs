use crate::core::types::valtype::ValType;

/// Global types classify global variables, which hold a value and can either be mutable or
/// immutable.
///
/// <https://www.w3.org/TR/wasm-core-2/#global-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-globaltype>
#[derive(Debug, PartialEq)]
pub struct GlobalType(pub Mut, pub ValType);

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Mut {
    Const,
    Var,
}
