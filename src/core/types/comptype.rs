use super::Mut;
use super::ValType;

/// Composite types are all types composed from simpler types, including function types, structure
/// types and array types.
///
/// <https://webassembly.github.io/spec/core/syntax/types.html#composite-types>
/// <https://webassembly.github.io/spec/core/binary/types.html#composite-types>
#[derive(Debug, PartialEq, Clone)]
pub enum CompType {
    Struct(Vec<FieldType>),
    Array(FieldType),
    Func {
        parameters: Vec<ValType>,
        results: Vec<ValType>,
    },
}

// TODO: do we have to define a separate Mut type for comptypes, or is it fine
// to reuse the one from globaltype?
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct FieldType(pub Mut, pub StorageType);

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum StorageType {
    ValType(ValType),
    PackType(PackType),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum PackType {
    I8,
    I16,
}
