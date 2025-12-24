pub mod types;

mod module;
pub use module::{Module, SectionHeader, SectionKind};

use crate::instructions::Instruction;
pub(crate) type Expr = Vec<Instruction>;
