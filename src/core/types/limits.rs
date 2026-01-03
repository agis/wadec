use super::addrtype::AddrType;

/// Limits classify the size range of resizeable storage associated with memory types and
/// table types. If no maximum is given, the respective storage can grow to any size.
///
/// <https://www.w3.org/TR/wasm-core-2/#limits>
/// <https://www.w3.org/TR/wasm-core-2/#binary-limits>
#[derive(Debug, PartialEq)]
pub struct Limits {
    pub address_type: AddrType,
    pub min: u64,
    pub max: Option<u64>,
}
