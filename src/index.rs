//! Types and constructors for Web Assembly indices,
//! as defined in Section 5.5.1.
//!
//! See <https://www.w3.org/TR/wasm-core-2/#indices>
use crate::integer::{decode_u32, DecodeU32Error};
use std::io::Read;
use thiserror::Error;

macro_rules! define_index {
    ($name:ident, $errorname:ident) => {
        #[derive(Debug, Error)]
        #[error("failed decoding {name} index", name = stringify!($name))]
        pub struct $errorname(#[from] pub DecodeU32Error);

        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(pub u32);

        impl $name {
            pub fn decode<R: Read + ?Sized>(r: &mut R) -> Result<Self, $errorname> {
                let idx = decode_u32(r)?;
                Ok(Self(idx))
            }
        }
    };
}

define_index!(TypeIdx, TypeIdxError);
define_index!(FuncIdx, FuncIdxError);
define_index!(TableIdx, TableIdxError);
define_index!(MemIdx, MemIdxError);
define_index!(GlobalIdx, GlobalIdxError);
define_index!(ElemIdx, ElemIdxError);
define_index!(DataIdx, DataIdxError);
define_index!(LocalIdx, LocalIdxError);
define_index!(LabelIdx, LabelIdxError);
