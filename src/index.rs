use crate::integer;
use std::io::Read;
use thiserror::Error;

macro_rules! define_index {
    ($name:ident, $errorname:ident) => {
        #[derive(Debug, Error)]
        #[error("failed decoding {name} index", name = stringify!($name))]
        pub struct $errorname(#[from] integer::DecodeU32Error);

        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(pub u32);

        impl $name {
            pub fn read<R: Read + ?Sized>(r: &mut R) -> Result<Self, $errorname> {
                let idx = integer::read_u32(r)?;
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
