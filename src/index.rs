use crate::parse_u32;
use anyhow::Result;
use std::io;

// https://webassembly.github.io/spec/core/syntax/modules.html#indices
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct TypeIdx(pub u32);

impl TypeIdx {
    pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
        Ok(Self(parse_u32(r)?))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FuncIdx(pub u32);

impl FuncIdx {
    pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
        Ok(Self(parse_u32(r)?))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct TableIdx(pub u32);

impl TableIdx {
    pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
        Ok(Self(parse_u32(r)?))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct MemIdx(pub u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct GlobalIdx(pub u32);
impl GlobalIdx {
    pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
        Ok(Self(parse_u32(r)?))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ElemIdx(pub u32);

impl ElemIdx {
    pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
        Ok(Self(parse_u32(r)?))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DataIdx(pub u32);

impl DataIdx {
    pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
        Ok(Self(parse_u32(r)?))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LocalIdx(pub u32);

impl LocalIdx {
    pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
        Ok(Self(parse_u32(r)?))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LabelIdx(pub u32);

impl LabelIdx {
    pub fn read<R: io::Read + ?Sized>(r: &mut R) -> Result<Self> {
        Ok(Self(parse_u32(r)?))
    }
}
