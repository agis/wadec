use crate::core::types::heaptype::{AbsHeapType, HeapType};
use crate::core::types::reftype::RefType;
use crate::decode::types::heaptype::{DecodeAbsHeapTypeError, DecodeHeapTypeError};
use crate::read_byte;
use std::io::Cursor;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeRefTypeError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    DecodeHeapType(#[from] DecodeHeapTypeError),

    #[error(transparent)]
    InvalidMarkerByte(#[from] DecodeAbsHeapTypeError),
}

impl RefType {
    pub(crate) fn decode<R: Read + ?Sized>(reader: &mut R) -> Result<Self, DecodeRefTypeError> {
        let marker = read_byte(reader)?;

        Ok(match marker {
            0x63 => Self {
                nullable: true,
                ht: HeapType::decode(reader)?,
            },
            0x64 => Self {
                nullable: false,
                ht: HeapType::decode(reader)?,
            },
            b => {
                let aht = {
                    let mut reader = Cursor::new([b]).chain(reader);
                    AbsHeapType::decode(&mut reader)?
                };

                Self {
                    nullable: true,
                    ht: HeapType::Ht(aht),
                }
            }
        })
    }
}
