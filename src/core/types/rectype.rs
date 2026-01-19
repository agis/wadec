use crate::core::indices::TypeIdx;
use crate::core::types::comptype::CompType;

/// Recursive types denote a group of mutually recursive composite types, each of which can
/// optionally declare a list of type uses of supertypes that it matches. Each type can also be
/// declared final, preventing further subtyping.
///
/// <https://webassembly.github.io/spec/core/syntax/types.html#recursive-types>
/// <https://webassembly.github.io/spec/core/binary/types.html#recursive-types>
#[derive(Debug, PartialEq, Clone)]
pub struct RecType(pub Vec<SubType>);

#[derive(Debug, PartialEq, Clone)]
pub struct SubType {
    pub is_final: bool,
    pub supertypes: Vec<TypeIdx>,
    pub comptype: CompType,
}
