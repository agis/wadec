//! Decoding for WebAssembly indices.
//!
//! See <https://www.w3.org/TR/wasm-core-2/#indices>

use crate::core::indices::*;
use crate::decode::integer::{decode_u32, DecodeU32Error};
use std::io::Read;
use thiserror::Error;

macro_rules! define_index_decoder {
    ($name:ident, $errorname:ident) => {
        #[derive(Debug, Error)]
        #[error("failed decoding {name} index", name = stringify!($name))]
        pub struct $errorname(#[from] pub DecodeU32Error);

        impl $name {
            #[allow(dead_code)]
            pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, $errorname> {
                let idx = decode_u32(reader)?;
                Ok(Self(idx))
            }
        }
    };
}

define_index_decoder!(TypeIdx, DecodeTypeIdxError);
define_index_decoder!(FuncIdx, DecodeFuncIdxError);
define_index_decoder!(TableIdx, DecodeTableIdxError);
define_index_decoder!(MemIdx, DecodeMemIdxError);
define_index_decoder!(GlobalIdx, DecodeGlobalIdxError);
define_index_decoder!(ElemIdx, DecodeElemIdxError);
define_index_decoder!(DataIdx, DecodeDataIdxError);
define_index_decoder!(LocalIdx, DecodeLocalIdxError);
define_index_decoder!(LabelIdx, DecodeLabelIdxError);
define_index_decoder!(TagIdx, DecodeTagIdxError);
