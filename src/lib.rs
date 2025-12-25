//! A decoder for WebAssembly modules in the binary format.
//!
//! This library implements the Binary format of the WebAssembly [specification],
//! version 2.
//!
//! The main entry point is the [`decode_module`] function.
//!
//! [specification]: https://www.w3.org/TR/wasm-core-2/
#![forbid(unsafe_code)]

pub mod core;
pub use core::Module;

pub mod decode;
pub mod decode_errors;
pub use decode::decode_module;

pub(crate) use core::Expr;
pub(crate) use decode::read_byte;
