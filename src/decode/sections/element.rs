use crate::Expr;
use crate::decode::helpers::{DecodeVectorError, ParseExpressionError};
use crate::decode::helpers::{decode_expr, decode_vector};
use crate::decode::types::DecodeRefTypeError;
use crate::indices;
use crate::indices::{FuncIdx, TableIdx};
use crate::instructions::Instruction;
use crate::integer::{DecodeU32Error, decode_u32};
use crate::types::reftype::RefType;
use std::io::Read;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeElementSectionError {
    #[error("failed decoding Element section")]
    DecodeVector(#[from] DecodeVectorError<DecodeElementError>),
}

pub(crate) fn decode_element_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Elem>, DecodeElementSectionError> {
    Ok(decode_vector(reader, parse_elem)?)
}

/// The initial contents of a table is uninitialized. Element segments can be used to
/// initialize a subrange of a table from a static vector of elements. The elems component
/// of a module defines a vector of element segments. Each element segment defines a
/// reference type and a corresponding list of constant element expressions. Element
/// segments have a mode that identifies them as either passive, active, or declarative. A
/// passive element segment's elements can be copied to a table using the table.init
/// instruction. An active element segment copies its elements into a table during
/// instantiation, as specified by a table index and a constant expression defining an
/// offset into that table. A declarative element segment is not available at runtime but
/// merely serves to forward-declare references that are formed in code with instructions
/// like ref.func. The offset is given by a constant expression. Element segments are
/// referenced through element indices.
///
/// <https://www.w3.org/TR/wasm-core-2/#element-segments>
/// <https://www.w3.org/TR/wasm-core-2/#element-section>
#[derive(Debug, PartialEq)]
pub struct Elem {
    pub r#type: RefType,
    pub init: Vec<Expr>,
    pub mode: ElemMode,
}

#[derive(Debug, PartialEq)]
pub enum ElemMode {
    Passive,
    Active { table: TableIdx, offset: Expr },
    Declarative,
}

#[derive(Debug, Error)]
pub enum DecodeElementError {
    #[error("failed decoding bitfield")]
    DecodeBitfield(DecodeU32Error),

    #[error("invalid bitfield: expected value in range [0,7]; got {0}")]
    InvalidBitfield(u32),

    #[error("failed decoding offset expression")]
    DecodeOffsetExpression(ParseExpressionError),

    #[error("failed decoding Element kind")]
    DecodeElementKind(#[from] DecodeElementKindError),

    #[error("failed decoding Element expression")]
    DecodeElementExpression(ParseExpressionError),

    #[error(transparent)]
    DecodeTableIdx(#[from] indices::TableIdxError),

    #[error(transparent)]
    DecodeReferenceType(#[from] DecodeRefTypeError),

    #[error("failed decoding table.init expressions")]
    DecodeInit(DecodeVectorError<ParseExpressionError>),

    #[error(transparent)]
    DecodeFuncIdxVector(#[from] DecodeVectorError<indices::FuncIdxError>),
}

fn parse_elem<R: Read + ?Sized>(reader: &mut R) -> Result<Elem, DecodeElementError> {
    let bitfield = decode_u32(reader).map_err(DecodeElementError::DecodeBitfield)?;

    fn funcidx_into_reffunc(idxs: Vec<FuncIdx>) -> Vec<Expr> {
        idxs.into_iter()
            .map(|idx| vec![Instruction::RefFunc(idx)])
            .collect()
    }

    let (r#type, init, mode) = match bitfield {
        0 => {
            let e = decode_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let y = decode_vector(reader, FuncIdx::decode)?;
            (
                RefType::Func,
                funcidx_into_reffunc(y),
                ElemMode::Active {
                    table: TableIdx(0),
                    offset: e,
                },
            )
        }
        1 => {
            let et = parse_elemkind(reader)?;
            let y = decode_vector(reader, FuncIdx::decode)?;
            (et, funcidx_into_reffunc(y), ElemMode::Passive)
        }
        2 => {
            let x = TableIdx::decode(reader)?;
            let e = decode_expr(reader).map_err(DecodeElementError::DecodeElementExpression)?;
            let et = parse_elemkind(reader)?;
            let y = decode_vector(reader, FuncIdx::decode)?;
            (
                et,
                funcidx_into_reffunc(y),
                ElemMode::Active {
                    table: x,
                    offset: e,
                },
            )
        }
        3 => {
            let et = parse_elemkind(reader)?;
            let y = decode_vector(reader, FuncIdx::decode)?;
            (et, funcidx_into_reffunc(y), ElemMode::Declarative)
        }
        4 => {
            let e = decode_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let el = decode_vector(reader, decode_expr).map_err(DecodeElementError::DecodeInit)?;
            (
                RefType::Func,
                el,
                ElemMode::Active {
                    table: TableIdx(0),
                    offset: e,
                },
            )
        }
        5 => {
            let et = RefType::decode(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = decode_vector(reader, decode_expr).map_err(DecodeElementError::DecodeInit)?;
            (et, el, ElemMode::Passive)
        }
        6 => {
            let x = TableIdx::decode(reader)?;
            let e = decode_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let et = RefType::decode(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = decode_vector(reader, decode_expr).map_err(DecodeElementError::DecodeInit)?;

            (
                et,
                el,
                ElemMode::Active {
                    table: x,
                    offset: e,
                },
            )
        }
        7 => {
            let et = RefType::decode(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = decode_vector(reader, decode_expr).map_err(DecodeElementError::DecodeInit)?;

            (et, el, ElemMode::Declarative)
        }
        n => return Err(DecodeElementError::InvalidBitfield(n)),
    };

    Ok(Elem { r#type, init, mode })
}

#[derive(Debug, Error)]
pub enum DecodeElementKindError {
    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error("expected byte 0x00; got {0:#04X}")]
    InvalidElemKind(u8),
}

fn parse_elemkind<R: Read + ?Sized>(reader: &mut R) -> Result<RefType, DecodeElementKindError> {
    // we intentionally don't use RefType::read, since the spec uses 0x00
    // to mean `funcref`, but only in the legacy Element encodings
    let b = crate::read_byte(reader)?;
    if b != 0x00 {
        return Err(DecodeElementKindError::InvalidElemKind(b));
    }
    Ok(RefType::Func)
}
