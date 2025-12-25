use super::Limits;
use crate::core::indices::MemIdx;
use crate::Expr;

/// Memory types classify linear memories and their size range. The limits constrain the
/// minimum and optionally the maximum size of a memory. The limits are given in units of
/// page size.
///
/// <https://www.w3.org/TR/wasm-core-2/#memory-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-memtype>
#[derive(Debug, PartialEq)]
pub struct MemType {
    pub limits: Limits,
}

/// The initial contents of a memory are zero bytes. Data segments can be used to initialize
/// a range of memory from a static vector of bytes. The datas component of a module
/// defines a vector of data segments. Like element segments, data segments have a mode
/// that identifies them as either passive or active. A passive data segment's contents can
/// be copied into a memory using the memory.init instruction. An active data segment
/// copies its contents into a memory during instantiation, as specified by a memory index
/// and a constant expression defining an offset into that memory. Data segments are
/// referenced through data indices.
///
/// <https://www.w3.org/TR/wasm-core-2/#data-segments>
/// <https://www.w3.org/TR/wasm-core-2/#data-section>
#[derive(Debug, PartialEq)]
pub struct Data {
    pub init: Vec<u8>,
    pub mode: DataMode,
}

#[derive(Debug, PartialEq)]
pub enum DataMode {
    Passive,
    Active { memory: MemIdx, offset: Expr },
}
