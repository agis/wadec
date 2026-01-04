use super::globaltype::GlobalType;
use super::memtype::MemType;
use super::tabletype::TableType;
use super::tagtype::TagType;
use crate::core::indices::TypeIdx;

/// External types classify imports and external addresses with their respective types.
/// <https://webassembly.github.io/spec/core/syntax/types.html#address-types>
#[derive(Debug, PartialEq)]
pub enum ExternType {
    Func(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
    Tag(TagType),
}
