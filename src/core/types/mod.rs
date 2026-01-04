//! Type definitions for core WebAssembly types.
//!
//! <https://www.w3.org/TR/wasm-core-2/#types>
pub mod functype;
pub use functype::FuncType;

pub mod globaltype;
pub use globaltype::{GlobalType, Mut};

pub mod limits;
pub use limits::Limits;

pub mod memtype;
pub use memtype::MemType;

pub mod numtype;
pub use numtype::NumType;

pub mod reftype;
pub use reftype::RefType;

pub mod resulttype;
pub use resulttype::ResultType;

pub mod tabletype;
pub use tabletype::TableType;

pub mod valtype;
pub use valtype::ValType;

pub mod vectype;
pub use vectype::VecType;

pub mod addrtype;
pub use addrtype::AddrType;

pub mod tagtype;
pub use tagtype::TagType;

pub mod externtype;
pub use externtype::ExternType;
