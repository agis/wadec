//! Heap types classify objects in the runtime store. There are three disjoint hierarchies of heap types:
//!
//! - function types classify functions,
//! - aggregate types classify dynamically allocated managed data,
//!   such as structures, arrays, or unboxed scalars,
//! - external types classify external references possibly owned by the embedder.
//!
//! The values from the latter two hierarchies are interconvertible by ways of the
//! 𝖾𝗑𝗍𝖾𝗋𝗇.𝖼𝗈𝗇𝗏𝖾𝗋𝗍_𝖺𝗇𝗒 and 𝖺𝗇𝗒.𝖼𝗈𝗇𝗏𝖾𝗋𝗍_𝖾𝗑𝗍𝖾𝗋𝗇 instructions. That is, both type hierarchies are
//! inhabited by an isomorphic set of values, but may have different, incompatible representations
//! in practice.
//!
//! A heap type is either abstract or concrete. A concrete heap type consists of a type use that classifies an object of the respective type defined in a module. Abstract types are denoted by individual keywords.
//!
//! The type 𝖿𝗎𝗇𝖼 denotes the common supertype of all function types, regardless of their concrete definition. Dually, the type 𝗇𝗈𝖿𝗎𝗇𝖼 denotes the common subtype of all function types, regardless of their concrete definition. This type has no values.
//!
//! The type 𝖾𝗑𝗇 denotes the common supertype of all exception references. This type has no concrete subtypes. Dually, the type 𝗇𝗈𝖾𝗑𝗇 denotes the common subtype of all forms of exception references. This type has no values.
//!
//! The type 𝖾𝗑𝗍𝖾𝗋𝗇 denotes the common supertype of all external references received through the embedder. This type has no concrete subtypes. Dually, the type 𝗇𝗈𝖾𝗑𝗍𝖾𝗋𝗇 denotes the common subtype of all forms of external references. This type has no values.
//!
//! The type 𝖺𝗇𝗒 denotes the common supertype of all aggregate types, as well as possibly abstract values produced by internalizing an external reference of type 𝖾𝗑𝗍𝖾𝗋𝗇. Dually, the type 𝗇𝗈𝗇𝖾 denotes the common subtype of all forms of aggregate types. This type has no values.
//!
//! The type 𝖾𝗊 is a subtype of 𝖺𝗇𝗒 that includes all types for which references can be compared, i.e., aggregate values and 𝗂𝟥𝟣.
//!
//! The types 𝗌𝗍𝗋𝗎𝖼𝗍 and 𝖺𝗋𝗋𝖺𝗒 denote the common supertypes of all structure and array aggregates, respectively.
//!
//! The type 𝗂𝟥𝟣 denotes unboxed scalars, that is, integers injected into references. Their observable value range is limited to 31 bits.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeapType {
    Abs(AbsHeapType),
    TypeIdx(i32), // WAYPOINT: should this be u32?
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbsHeapType {
    Exn,
    Array,
    Struct,
    I31,
    Eq,
    Any,
    Extern,
    Func,
    None,
    NoExtern,
    NoFunc,
    NoExn,
}
