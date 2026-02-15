use crate::core::types::reftype::RefType;
use crate::core::types::tabletype::TableType;
use crate::decode::types::limits::ParseLimitsError;
use crate::decode::types::limits::parse_limits;
use crate::decode::types::reftype::DecodeRefTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTableTypeError {
    #[error(transparent)]
    DecodeRefType(#[from] DecodeRefTypeError),

    #[error(transparent)]
    DecodeLimits(#[from] ParseLimitsError),
}

impl TableType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeTableTypeError> {
        let reftype = RefType::decode(reader)?;
        let limits = parse_limits(reader)?;
        Ok(TableType { reftype, limits })
    }
}
