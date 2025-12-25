pub mod custom_section;
pub mod indices;
pub mod instruction;
mod module;
pub mod types;

pub use module::{Module, SectionHeader, SectionKind};
pub(crate) type Expr = Vec<instruction::Instruction>;
