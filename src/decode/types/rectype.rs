use crate::core::indices::TypeIdx;
use crate::core::types::comptype::CompType;
use crate::core::types::rectype::*;
use crate::decode::helpers::{DecodeListError, decode_list, read_byte};
use crate::decode::indices::DecodeTypeIdxError;
use crate::decode::types::comptype::DecodeCompTypeError;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeRecTypeError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("failed decoding subtypes list")]
    DecodeSubtypes(#[from] DecodeListError<DecodeSubTypeError>),

    #[error("failed decoding subtype")]
    DecodeSubtype(#[from] DecodeSubTypeError),
}

impl RecType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeRecTypeError> {
        let marker = read_byte(reader)?;

        Ok(if marker == 0x4E {
            let subtypes = decode_list(reader, SubType::decode)?;
            Self(subtypes)
        } else {
            // rewind reader since we already consumed the marker byte
            let mut reader = io::Cursor::new([marker]).chain(reader);
            let st = SubType::decode(&mut reader)?;
            Self(vec![st])
        })
    }
}

/*************************/
/*        SubType        */
/*************************/
#[derive(Debug, Error)]
pub enum DecodeSubTypeError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("failed decoding supertypes")]
    DecodeSupertypes(#[from] DecodeListError<DecodeTypeIdxError>),

    #[error("failed decoding recursive subtype")]
    DecodeCompType(#[from] DecodeCompTypeError),
}

impl SubType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeSubTypeError> {
        let marker = read_byte(reader)?;

        match marker {
            op @ (0x4F | 0x50) => {
                let x = decode_list(reader, TypeIdx::decode)?;
                let ct = CompType::decode(reader)?;
                let is_final = op == 0x4F;

                Ok(Self {
                    is_final,
                    supertypes: x,
                    comptype: ct,
                })
            }
            _ => {
                // we consumed the marker byte already, so we have to rewind the
                // reader to try and parse it as comptype
                let mut reader = io::Cursor::new([marker]).chain(reader);

                Ok(Self {
                    is_final: true,
                    supertypes: vec![],
                    comptype: CompType::decode(&mut reader)?,
                })
            }
        }
    }
}
