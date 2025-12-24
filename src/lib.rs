//! A decoder for WebAssembly modules in the binary format.
//!
//! This library implements the Binary format of the WebAssembly [specification],
//! version 2.
//!
//! The main entry point is the [`decode()`] function.
//!
//! [specification]: https://www.w3.org/TR/wasm-core-2/
#![forbid(unsafe_code)]

mod core;
pub use core::instruction::{BlockType, Instruction, LaneIdx, Memarg};
pub use core::types;
pub use core::{Module, SectionHeader, SectionKind};

pub mod decode;
pub use decode::instructions::{
    BlockTypeError, ControlError, LaneIdxError, MemargError, MemoryError, NumericError,
    ParametricError, ParseError, ReferenceError, TableError, VariableError, VectorError,
};
pub(crate) use decode::read_byte;
pub use decode::{
    DecodeModuleError, DecodeSectionHeaderError, InvalidSectionIdError, ParsePreambleError, decode,
    integer,
};

pub mod indices;
pub(crate) use core::Expr;
