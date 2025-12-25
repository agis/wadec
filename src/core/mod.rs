pub mod custom_section;
pub mod indices;
pub mod instruction;
mod module;
pub mod types;

use instruction::Instruction;
pub use module::{Module, SectionHeader, SectionKind};
pub(crate) type Expr = Vec<Instruction>;
