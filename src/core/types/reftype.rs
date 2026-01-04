use super::heaptype::HeapType;

/// Reference types classify first-class references to objects in the runtime store.
///
/// A reference type is characterized by a heap type and a nullability flag.
///
/// <https://webassembly.github.io/spec/core/syntax/types.html#reference-types>
/// <https://webassembly.github.io/spec/core/binary/types.html#reference-types>
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct RefType {
    pub nullable: bool,
    pub ht: HeapType,
}
