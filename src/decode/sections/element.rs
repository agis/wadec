use crate::Expr;
use crate::core::indices::{FuncIdx, TableIdx};
use crate::core::instruction::Instruction;
use crate::core::types::heaptype::{AbsHeapType, HeapType};
use crate::core::types::reftype::RefType;
use crate::core::{Elem, ElemMode};
use crate::decode::helpers::{DecodeListError, ParseExpressionError};
use crate::decode::helpers::{decode_expr, decode_list};
use crate::decode::indices::{DecodeFuncIdxError, DecodeTableIdxError};
use crate::decode::integer::{DecodeU32Error, decode_u32};
use crate::decode::types::DecodeRefTypeError;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeElementSectionError {
    #[error("failed decoding Element section")]
    DecodeVector(#[from] DecodeListError<DecodeElementError>),
}

pub(crate) fn decode_element_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Elem>, DecodeElementSectionError> {
    Ok(decode_list(reader, parse_elem)?)
}

/*************************/
/*       Element         */
/*************************/

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
    DecodeTableIdx(#[from] DecodeTableIdxError),

    #[error(transparent)]
    DecodeReferenceType(#[from] DecodeRefTypeError),

    #[error("failed decoding table.init expressions")]
    DecodeInit(DecodeListError<ParseExpressionError>),

    #[error(transparent)]
    DecodeFuncIdxVector(#[from] DecodeListError<DecodeFuncIdxError>),
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
            let (e, _) = decode_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let y = decode_list(reader, FuncIdx::decode)?;
            (
                RefType {
                    nullable: false,
                    ht: HeapType::Ht(AbsHeapType::Func),
                },
                funcidx_into_reffunc(y),
                ElemMode::Active {
                    table: TableIdx(0),
                    offset: e,
                },
            )
        }
        1 => {
            let rt = parse_elemkind(reader)?;
            let y = decode_list(reader, FuncIdx::decode)?;
            (rt, funcidx_into_reffunc(y), ElemMode::Passive)
        }
        2 => {
            let x = TableIdx::decode(reader)?;
            let (e, _) =
                decode_expr(reader).map_err(DecodeElementError::DecodeElementExpression)?;
            let rt = parse_elemkind(reader)?;
            let y = decode_list(reader, FuncIdx::decode)?;
            (
                rt,
                funcidx_into_reffunc(y),
                ElemMode::Active {
                    table: x,
                    offset: e,
                },
            )
        }
        3 => {
            let rt = parse_elemkind(reader)?;
            let y = decode_list(reader, FuncIdx::decode)?;
            (rt, funcidx_into_reffunc(y), ElemMode::Declare)
        }
        4 => {
            let (e, _) = decode_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let el = decode_list(reader, |r| decode_expr(r).map(|(expr, _)| expr))
                .map_err(DecodeElementError::DecodeInit)?;
            (
                RefType {
                    nullable: true,
                    ht: HeapType::Ht(AbsHeapType::Func),
                },
                el,
                ElemMode::Active {
                    table: TableIdx(0),
                    offset: e,
                },
            )
        }
        5 => {
            let rt = RefType::decode(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = decode_list(reader, |r| decode_expr(r).map(|(expr, _)| expr))
                .map_err(DecodeElementError::DecodeInit)?;
            (rt, el, ElemMode::Passive)
        }
        6 => {
            let x = TableIdx::decode(reader)?;
            let (e, _) = decode_expr(reader).map_err(DecodeElementError::DecodeOffsetExpression)?;
            let rt = RefType::decode(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = decode_list(reader, |r| decode_expr(r).map(|(expr, _)| expr))
                .map_err(DecodeElementError::DecodeInit)?;

            (
                rt,
                el,
                ElemMode::Active {
                    table: x,
                    offset: e,
                },
            )
        }
        7 => {
            let rt = RefType::decode(reader).map_err(DecodeElementError::DecodeReferenceType)?;
            let el = decode_list(reader, |r| decode_expr(r).map(|(expr, _)| expr))
                .map_err(DecodeElementError::DecodeInit)?;

            (rt, el, ElemMode::Declare)
        }
        n => return Err(DecodeElementError::InvalidBitfield(n)),
    };

    Ok(Elem { r#type, init, mode })
}

/*************************/
/*       ElemKind        */
/*************************/

#[derive(Debug, Error)]
pub enum DecodeElementKindError {
    #[error(transparent)]
    Io(#[from] io::Error),

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

    Ok(RefType {
        nullable: true,
        ht: HeapType::Ht(AbsHeapType::Func),
    })
}
