//! Helpers for decoding WebAssembly values, types, instructions and sections.
pub(crate) mod helpers;
pub mod indices;
pub mod instructions;
pub mod integer;
mod module;
pub mod sections;
pub mod types;

pub(crate) use helpers::read_byte;
pub use helpers::{
    DecodeByteVectorError, DecodeFloat32Error, DecodeFloat64Error, DecodeNameError,
    DecodeVectorError, ParseExpressionError,
};
pub use module::{
    DecodeModuleError, DecodeSectionHeaderError, InvalidSectionIdError, ParsePreambleError,
    decode_module,
};

pub(crate) trait FromMarkerByte
where
    Self: Sized + Copy + std::fmt::Debug + 'static,
{
    type Error: From<u8>;

    // defines the mapping between expected bytes and the corresponding value type
    fn markers() -> &'static phf::OrderedMap<u8, Self>;

    fn markers_formatted() -> String {
        Self::markers()
            .entries()
            .map(|(marker, variant)| format!("{marker:#04X} ({variant:?})"))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn from_marker(b: u8) -> Result<Self, Self::Error> {
        match Self::markers().get(&b) {
            Some(n) => Ok(*n),
            None => Err(b.into()),
        }
    }
}
