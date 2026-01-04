//! Type definitions for WebAssembly indices.
//!
//! See <https://www.w3.org/TR/wasm-core-2/#indices>

macro_rules! define_index_type {
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(pub u32);
    };
}

define_index_type!(TypeIdx);
define_index_type!(FuncIdx);
define_index_type!(TableIdx);
define_index_type!(MemIdx);
define_index_type!(GlobalIdx);
define_index_type!(ElemIdx);
define_index_type!(DataIdx);
define_index_type!(LocalIdx);
define_index_type!(LabelIdx);
define_index_type!(TagIdx);
