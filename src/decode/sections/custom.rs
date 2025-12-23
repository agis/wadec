use crate::decode::helpers::{decode_name, DecodeNameError};
use std::io::{self, Read};
use thiserror::Error;

/// Custom sections have the id 0. They are intended to be used for debugging information or
/// third-party extensions, and are ignored by the WebAssembly semantics. Their contents consist of
/// a name further identifying the custom section, followed by an uninterpreted sequence of bytes
/// for custom use.
///
/// <https://www.w3.org/TR/wasm-core-2/#custom-section>
/// <https://www.w3.org/TR/wasm-core-2/#binary-customsec>
#[derive(Debug, PartialEq)]
pub struct CustomSection {
    pub name: String,
    pub contents: Vec<u8>,
}

#[derive(Debug, Error)]
pub enum DecodeCustomSectionError {
    #[error("failed decoding custom section name")]
    DecodeName(#[from] DecodeNameError),

    #[error("failed reading custom section contents")]
    Io(#[from] io::Error),
}

pub(crate) fn decode_section_custom<R: Read + ?Sized>(
    reader: &mut R,
) -> Result<CustomSection, DecodeCustomSectionError> {
    let name = decode_name(reader)?;
    let mut contents = Vec::new();
    reader.read_to_end(&mut contents)?;

    Ok(CustomSection { name, contents })
}
