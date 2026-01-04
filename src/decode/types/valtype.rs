use crate::core::types::numtype::NumType;
use crate::core::types::reftype::RefType;
use crate::core::types::valtype::ValType;
use crate::core::types::vectype::VecType;
use crate::decode::types::reftype::DecodeRefTypeError;
use crate::read_byte;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeValTypeError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    DecodeRefType(#[from] DecodeRefTypeError),
}

impl ValType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeValTypeError> {
        Ok(match read_byte(reader)? {
            0x7F => ValType::Num(NumType::Int32),
            0x7E => ValType::Num(NumType::Int64),
            0x7D => ValType::Num(NumType::Float32),
            0x7C => ValType::Num(NumType::Float64),
            0x7B => ValType::Vec(VecType::V128),
            b => {
                let mut reader = io::Cursor::new([b]).chain(reader);
                ValType::Ref(RefType::decode(&mut reader)?)
            }
        })
    }
}
