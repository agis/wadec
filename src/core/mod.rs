pub mod custom_section;
pub mod instruction;
pub mod types;

mod module;
pub use module::{Module, SectionHeader, SectionKind};

pub(crate) type Expr = Vec<instruction::Instruction>;
