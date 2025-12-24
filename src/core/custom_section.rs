//! WebAssembly custom section representation.
//!
//! <https://www.w3.org/TR/wasm-core-2/#custom-section>
//! <https://www.w3.org/TR/wasm-core-2/#binary-customsec>
#[derive(Debug, PartialEq)]
pub struct CustomSection {
    pub name: String,
    pub contents: Vec<u8>,
}
