use crate::decode::helpers::{DecodeNameError, decode_name};
pub use crate::core::custom_section::CustomSection;
use std::io::{self, Read};
use thiserror::Error;

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
