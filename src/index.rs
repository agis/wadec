use crate::integer;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("failed to decode {kind:?}")]
pub struct Error {
    pub kind: ErrorKind,
    pub source: integer::DecodeError,
}

macro_rules! define_index {
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(pub u32);

        impl $name {
            pub fn read<R: Read + ?Sized>(r: &mut R) -> Result<Self, Error> {
                let idx = integer::read_u32(r).map_err(|source| Error {
                    kind: ErrorKind::$name,
                    source,
                })?;

                Ok(Self(idx))
            }
        }
    };
}

macro_rules! define_indices {
    ($($name:ident),+ $(,)?) => {
        #[derive(Debug)]
        pub enum ErrorKind { $( $name ),+ }

        $( define_index!($name); )+
    };
}

define_indices!(
    TypeIdx, FuncIdx, TableIdx, MemIdx, GlobalIdx, ElemIdx, DataIdx, LocalIdx, LabelIdx,
);
