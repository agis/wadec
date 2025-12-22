use crate::decode::helpers::{decode_name, decode_vector, DecodeNameError, DecodeVectorError};
use crate::indices::{FuncIdx, GlobalIdx, MemIdx, TableIdx};
use crate::integer::{decode_u32, DecodeU32Error};
use crate::read_byte;
use std::io;
use std::io::Read;
use thiserror::Error;

/// The exports component of a module defines a set of exports that become accessible to the
/// host environment once the module has been instantiated. Each export is labeled by a
/// unique name. Exportable definitions are functions, tables, memories, and globals, which
/// are referenced through a respective descriptor.
///
/// <https://www.w3.org/TR/wasm-core-2/#exports>
/// <https://www.w3.org/TR/wasm-core-2/#export-section>
#[derive(Debug, PartialEq)]
pub struct Export {
    pub name: String,
    pub desc: ExportDesc,
}

#[derive(Debug, Error)]
pub enum DecodeExportSectionError {
    #[error("failed decoding Export section")]
    DecodeVector(#[from] DecodeVectorError<DecodeExportError>),
}

pub(crate) fn decode_export_section<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<Vec<Export>, DecodeExportSectionError> {
    Ok(decode_vector(reader, parse_export)?)
}

#[derive(Debug, PartialEq)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug, Error)]
pub enum DecodeExportError {
    #[error(transparent)]
    DecodeName(#[from] DecodeNameError),

    #[error("failed reading Export descriptor marker byte")]
    ReadDescriptorMarkerByte(io::Error),

    #[error("failed reading ExportDesc index")]
    DecodeIndex(#[from] DecodeU32Error),

    #[error(transparent)]
    InvalidDescriptorMarkerByte(#[from] InvalidExportDescMarkerByte),
}

// TODO: validate that names are unique?
fn parse_export<R: Read + ?Sized>(reader: &mut R) -> Result<Export, DecodeExportError> {
    let name = decode_name(reader)?;

    let desc_kind = read_byte(reader).map_err(DecodeExportError::ReadDescriptorMarkerByte)?;
    let idx = decode_u32(reader)?;
    let desc = ExportDesc::from(desc_kind, idx)?;

    Ok(Export { name, desc })
}

#[derive(Debug, Error)]
#[error(
    "invalid ExportDesc marker byte: expected 0x00 (func), 0x01 (table), 0x02 (mem) or 0x03 (global); got {0:#04X}"
)]
pub struct InvalidExportDescMarkerByte(pub u8);

impl ExportDesc {
    fn from(b: u8, idx: u32) -> Result<Self, InvalidExportDescMarkerByte> {
        Ok(match b {
            0x00 => ExportDesc::Func(FuncIdx(idx)),
            0x01 => ExportDesc::Table(TableIdx(idx)),
            0x02 => ExportDesc::Mem(MemIdx(idx)),
            0x03 => ExportDesc::Global(GlobalIdx(idx)),
            _ => return Err(InvalidExportDescMarkerByte(b)),
        })
    }
}
