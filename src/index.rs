use crate::integer;
use anyhow::Result;
use std::io;

macro_rules! define_index {
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(pub u32);

        impl $name {
            pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
                Ok(Self(integer::read_u32(r)?))
            }
        }
    };
}

define_index!(TypeIdx);
define_index!(FuncIdx);
define_index!(TableIdx);
define_index!(MemIdx);
define_index!(GlobalIdx);
define_index!(ElemIdx);
define_index!(DataIdx);
define_index!(LocalIdx);
define_index!(LabelIdx);
