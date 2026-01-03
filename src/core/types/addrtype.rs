/// Address types are a subset of number types that classify the values that can be used as offsets
/// into memories and tables.
///
/// <https://webassembly.github.io/spec/core/syntax/types.html#address-types>
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum AddrType {
    I32,
    I64,
}
