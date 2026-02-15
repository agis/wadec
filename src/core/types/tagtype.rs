use crate::core::indices::TypeIdx;

/// Tag types classify the signature tags with a type use referring to the definition of a function
/// type that declares the types of parameter and result values associated with the tag. The result
/// type is empty for exception tags.
///
/// <https://webassembly.github.io/spec/core/syntax/types.html#tag-types>
/// <https://webassembly.github.io/spec/core/binary/types.html#tag-types>
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct TagType(pub TypeIdx);
