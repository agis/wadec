use crate::core::types::limits::Limits;

/// Memory types classify linear memories and their size range. The limits constrain the
/// minimum and optionally the maximum size of a memory. The limits are given in units of
/// page size.
///
/// <https://www.w3.org/TR/wasm-core-2/#memory-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-memtype>
#[derive(Debug, PartialEq)]
pub struct MemType {
    pub limits: Limits,
}
