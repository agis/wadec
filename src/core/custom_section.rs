//! WebAssembly custom section type definition.

/// Custom sections  are intended to be used for debugging information or
/// third-party extensions, and are ignored by the WebAssembly semantics.
///
/// Their contents consist of a name further identifying the custom section, followed by an
/// uninterpreted sequence of bytes
/// for custom use
///
/// <https://www.w3.org/TR/wasm-core-2/#custom-section>
/// <https://www.w3.org/TR/wasm-core-2/#binary-customsec>
#[derive(Debug, PartialEq)]
pub struct CustomSection {
    pub name: String,
    pub contents: Vec<u8>,
}
