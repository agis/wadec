use crate::core::types::tabletype::TableType;
use crate::core::Instruction;
use crate::core::Table;
use crate::decode::helpers::{decode_expr, decode_list, read_byte, DecodeListError};
use crate::decode::types::DecodeTableTypeError;
use crate::decode::ParseExpressionError;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeTableSectionError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("failed decoding Table section")]
    DecodeVector(#[from] DecodeListError<DecodeTableError>),

    #[error("expected 0x00 after 0x40; got {0:#04X}")]
    UnexpectedByte(u8),
}

pub(crate) fn decode_table_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Table>, DecodeTableSectionError> {
    Ok(decode_list(reader, decode_table)?)
}

/*************************/
/*         Table         */
/*************************/
#[derive(Debug, Error)]
pub enum DecodeTableError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("unexpected marker byte: expected 0x00; got {0:#04X}")]
    UnexpectedMarkerByte(u8),

    #[error(transparent)]
    DecodeTableType(#[from] DecodeTableTypeError),

    #[error(transparent)]
    DecodeExpression(#[from] ParseExpressionError),
}

fn decode_table<R: Read + ?Sized>(reader: &mut R) -> Result<Table, DecodeTableError> {
    let byte = read_byte(reader)?;
    if byte == 0x40 {
        let b = read_byte(reader)?;
        if b != 0x00 {
            return Err(DecodeTableError::UnexpectedMarkerByte(b));
        }
        let tt = TableType::decode(reader)?;
        let e = decode_expr(reader)?;
        return Ok(Table(tt, e));
    }

    // we consumed the first byte already, so we have to rewind the
    // reader to try and parse it as a TableType
    let mut reader = io::Cursor::new([byte]).chain(reader);
    let tt = TableType::decode(&mut reader)?;
    let e = vec![Instruction::RefNull(tt.reftype.ht)];
    Ok(Table(tt, e))
}
