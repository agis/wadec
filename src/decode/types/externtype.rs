use crate::core::indices::TypeIdx;
use crate::core::types::externtype::ExternType;
use crate::core::types::globaltype::GlobalType;
use crate::core::types::tabletype::TableType;
use crate::core::types::tagtype::TagType;
use crate::decode::indices::DecodeTypeIdxError;
use crate::decode::types::globaltype::DecodeGlobalTypeError;
use crate::decode::types::memtype::DecodeMemoryTypeError;
use crate::decode::types::memtype::parse_memtype;
use crate::decode::types::tabletype::DecodeTableTypeError;
use crate::decode::types::tagtype::DecodeTagTypeError;
use crate::read_byte;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeExternTypeError {
    #[error(transparent)]
    ReadMarkerByte(#[from] io::Error),

    #[error(
        "unexpected ExternType marker byte: expected 0x00, 0x01, 0x02, 0x03 or 0x04; got {0:#04X}"
    )]
    InvalidMarkerByte(u8),

    #[error(transparent)]
    DecodeTypeIndex(#[from] DecodeTypeIdxError),

    #[error(transparent)]
    DecodeTableType(#[from] DecodeTableTypeError),

    #[error(transparent)]
    DecodeMemoryType(#[from] DecodeMemoryTypeError),

    #[error(transparent)]
    DecodeGlobalType(#[from] DecodeGlobalTypeError),

    #[error(transparent)]
    DecodeTagType(#[from] DecodeTagTypeError),
}

impl ExternType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeExternTypeError> {
        let marker = read_byte(reader)?;
        match marker {
            0x00 => Ok(ExternType::Func(TypeIdx::decode(reader)?)),
            0x01 => Ok(ExternType::Table(TableType::decode(reader)?)),
            0x02 => Ok(ExternType::Mem(parse_memtype(reader)?)),
            0x03 => Ok(ExternType::Global(GlobalType::decode(reader)?)),
            0x04 => Ok(ExternType::Tag(TagType::decode(reader)?)),
            _ => Err(DecodeExternTypeError::InvalidMarkerByte(marker)),
        }
    }
}
