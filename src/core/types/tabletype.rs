use crate::core::types::limits::Limits;
use crate::core::types::reftype::RefType;

/// Table types classify tables over elements of reference type within a size range. Like
/// memories, tables are constrained by limits for their minimum and optionally maximum
/// size. The limits are given in numbers of entries.
///
/// <https://www.w3.org/TR/wasm-core-2/#table-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-tabletype>
#[derive(Debug, PartialEq)]
pub struct TableType {
    pub reftype: RefType,
    pub limits: Limits,
}
