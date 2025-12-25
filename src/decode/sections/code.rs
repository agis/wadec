use crate::core::types::valtype::ValType;
use crate::decode::helpers::{DecodeVectorError, ParseExpressionError};
use crate::decode::helpers::{decode_expr, decode_vector};
use crate::decode::integer::{DecodeU32Error, decode_u32};
use crate::decode::types::DecodeValTypeError;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeCodeSectionError {
    #[error("failed decoding Code section")]
    DecodeVector(#[from] DecodeVectorError<DecodeCodeError>),
}

pub(crate) fn decode_code_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Code>, DecodeCodeSectionError> {
    Ok(decode_vector(reader, parse_code)?)
}

#[derive(Debug, PartialEq)]
pub(crate) struct Code {
    size: u32,
    pub(crate) locals: Vec<Local>,
    pub(crate) expr: crate::Expr,
}

#[derive(Debug, Error)]
pub enum DecodeCodeError {
    #[error("failed decoding size of function code")]
    DecodeFunctionSize(DecodeU32Error),

    #[error("failed decoding locals vector")]
    DecodeLocalsVector(#[from] DecodeVectorError<DecodeCodeLocalsError>),

    #[error("failed decoding function body expression")]
    DecodeFunctionBody(#[from] ParseExpressionError),

    #[error(
        "Code entry size mismatch: declared {declared_bytes} bytes; consumed {consumed_bytes} (leftover: {leftover_bytes})"
    )]
    EntrySizeMismatch {
        declared_bytes: u32,
        leftover_bytes: u64,
        consumed_bytes: u64,
    },
}

fn parse_code<R: Read + ?Sized>(reader: &mut R) -> Result<Code, DecodeCodeError> {
    let size = decode_u32(reader).map_err(DecodeCodeError::DecodeFunctionSize)?;

    let mut reader = reader.take(size.into());
    let mut expanded_locals: u64 = 0;
    let max_locals = u64::from(u32::MAX);

    let locals = decode_vector(&mut reader, |r| {
        parse_code_local(r, &mut expanded_locals, max_locals)
    })?;

    let expr = decode_expr(&mut reader)?;

    if reader.limit() != 0 {
        return Err(DecodeCodeError::EntrySizeMismatch {
            declared_bytes: size,
            leftover_bytes: reader.limit(),
            consumed_bytes: u64::from(size) - reader.limit(),
        });
    }

    Ok(Code { size, locals, expr })
}

#[derive(Debug, PartialEq)]
pub(crate) struct Local {
    pub(crate) count: u32,
    pub(crate) t: ValType,
}

#[derive(Debug, Error)]
pub enum DecodeCodeLocalsError {
    #[error("failed decoding count of function locals")]
    DecodeLocalsCount(DecodeU32Error),

    #[error("too many locals: expected at most {max_locals}; got {actual_locals}")]
    LocalsCountOutOfBound { max_locals: u64, actual_locals: u64 },

    #[error("failed decoding local Value type")]
    DecodeLocalValType(#[from] DecodeValTypeError),
}

fn parse_code_local<R: Read + ?Sized>(
    reader: &mut R,
    expanded_locals: &mut u64,
    max_locals: u64,
) -> Result<Local, DecodeCodeLocalsError> {
    let count = decode_u32(reader).map_err(DecodeCodeLocalsError::DecodeLocalsCount)?;

    *expanded_locals += u64::from(count);
    if *expanded_locals > max_locals {
        return Err(DecodeCodeLocalsError::LocalsCountOutOfBound {
            max_locals,
            actual_locals: *expanded_locals,
        });
    }

    Ok(Local {
        count,
        t: ValType::decode(reader)?,
    })
}
