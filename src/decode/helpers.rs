use crate::Expr;
use crate::instructions::{self, Instruction};
use crate::integer::{DecodeU32Error, decode_u32};
use std::io;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseExpressionError {
    #[error("failed parsing instruction")]
    ParseInstruction(#[from] instructions::ParseError),

    #[error("unexpected Else delimiter")]
    UnexpectedElse,
}

pub(crate) fn decode_expr<R: Read + ?Sized>(reader: &mut R) -> Result<Expr, ParseExpressionError> {
    let mut body = Vec::new();

    loop {
        match Instruction::parse(reader)? {
            instructions::ParseResult::Instruction(ins) => body.push(ins),
            instructions::ParseResult::End => break,

            // `Else` is only expected to appear when parsing individual `Control` instructions
            instructions::ParseResult::Else => return Err(ParseExpressionError::UnexpectedElse),
        }
    }

    Ok(body)
}

#[derive(Debug, Error)]
pub enum DecodeFloat32Error {
    #[error("failed reading 4 bytes for f32")]
    ReadPayload(#[from] io::Error),
}

pub(crate) fn decode_f32<R: Read + ?Sized>(r: &mut R) -> Result<f32, DecodeFloat32Error> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf)?;
    Ok(f32::from_le_bytes(buf))
}

#[derive(Debug, Error)]
pub enum DecodeFloat64Error {
    #[error("failed reading 8 bytes for f64")]
    ReadPayload(#[from] io::Error),
}

pub(crate) fn decode_f64<R: Read + ?Sized>(r: &mut R) -> Result<f64, DecodeFloat64Error> {
    let mut buf = [0u8; 8];
    r.read_exact(&mut buf)?;
    Ok(f64::from_le_bytes(buf))
}

#[derive(Error)]
pub enum DecodeVectorError<E> {
    #[error("failed decoding vector length")]
    DecodeLength(#[from] DecodeU32Error),

    #[error("failed parsing vector element at position {position}")]
    ParseElement { position: u32, source: E },
}

pub(crate) fn decode_vector<R, F, T, E>(
    reader: &mut R,
    mut parse_fn: F,
) -> Result<Vec<T>, DecodeVectorError<E>>
where
    R: Read + ?Sized,
    F: FnMut(&mut R) -> Result<T, E>,
{
    let len = decode_u32(reader)?;

    let mut items = Vec::with_capacity(len.try_into().unwrap());
    for i in 0..len {
        let elem = parse_fn(reader).map_err(|err| DecodeVectorError::ParseElement {
            position: i,
            source: err,
        })?;
        items.push(elem);
    }

    Ok(items)
}

// we want any DecodeVectorError::ParseElement errors to also display the inner
// error type pointed to by source.
impl<E: std::fmt::Debug> std::fmt::Debug for DecodeVectorError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DecodeLength(e) => f.debug_tuple("DecodeLength").field(e).finish(),
            Self::ParseElement { position, source } => f
                .debug_struct("ParseElement")
                .field("position", position)
                .field(
                    "source",
                    &format_args!(
                        "{}::{source:#?}",
                        std::any::type_name::<E>()
                            .rsplit("::")
                            .next()
                            .unwrap_or_else(|| std::any::type_name::<E>())
                    ),
                )
                .finish(),
        }
    }
}

#[derive(Debug, Error)]
pub enum DecodeByteVectorError {
    #[error("failed decoding vector length")]
    DecodeLength(#[from] DecodeU32Error),

    #[error("failed reading vector elements")]
    ReadElements(#[from] io::Error),
}

pub(crate) fn decode_byte_vector<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<u8>, DecodeByteVectorError> {
    let len = decode_u32(reader)?;
    let mut b = vec![0u8; len.try_into().unwrap()];
    reader.read_exact(&mut b)?;
    Ok(b)
}

#[derive(Debug, Error)]
pub enum DecodeNameError {
    #[error(transparent)]
    DecodeByteVector(#[from] DecodeByteVectorError),

    #[error(transparent)]
    Utf8(#[from] std::string::FromUtf8Error),
}

pub(crate) fn decode_name<R: Read + ?Sized>(reader: &mut R) -> Result<String, DecodeNameError> {
    Ok(decode_byte_vector(reader)?.try_into()?)
}
