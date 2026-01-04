//! Type definitions for data structures representing a WebAssembly module and its
//! components.

pub mod custom_section;
pub use custom_section::CustomSection;

mod module;
pub use module::{Module, SectionHeader, SectionKind};

pub mod indices;
use indices::*;

pub mod instruction;
pub use instruction::Instruction;

pub mod types;
use types::ExternType;
use types::{GlobalType, RefType, ValType};

pub(crate) type Expr = Vec<Instruction>;

/// The imports component of a module defines a set of imports that are required for
/// instantiation. Each import is labeled by a two-level name space, consisting of a module name
/// and an item name for an entity within that module. Importable definitions are tags, globals,
/// memories, tables, and functions. Each import is specified by a respective external type that a
/// definition provided during instantiation is required to match.
///
/// Every import defines an index in the respective index space. In each index space, the indices
/// of imports go before the first index of any definition contained in the module itself.
///
/// <https://webassembly.github.io/spec/core/syntax/modules.html#syntax-import>
/// <https://webassembly.github.io/spec/core/binary/modules.html#binary-importsec>
#[derive(Debug, PartialEq)]
pub struct Import {
    pub module_name: String,
    pub item_name: String,
    pub extern_type: ExternType,
}

/// The exports component of a module defines a set of exports that become accessible to the
/// host environment once the module has been instantiated. Each export is labeled by a
/// unique name. Exportable definitions are functions, tables, memories, and globals, which
/// are referenced through a respective descriptor.
///
/// <https://www.w3.org/TR/wasm-core-2/#exports>
/// <https://www.w3.org/TR/wasm-core-2/#export-section>
#[derive(Debug, PartialEq)]
pub struct Export {
    pub name: String,
    pub externidx: ExternIdx,
}

#[derive(Debug, PartialEq)]
pub enum ExternIdx {
    Func(FuncIdx),
    Global(GlobalIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Tag(TagIdx),
}

/// The funcs component of a module defines a vector of functions.
///
/// Functions are referenced through function indices, starting with the
/// smallest index not referencing a function import.
///
/// <https://www.w3.org/TR/wasm-core-2/#functions>
/// <https://www.w3.org/TR/wasm-core-2/#code-section>
#[derive(Debug, PartialEq)]
pub struct Func {
    /// The type of a function declares its signature by reference to a type defined
    /// in the module. The parameters of the function are referenced through 0-based local indices
    /// in the function's body; they are mutable.
    pub r#type: TypeIdx,

    /// The locals declare a vector of mutable local variables and their types.
    /// These variables are referenced through local indices in the function's
    /// body. The index of the first local is the smallest index not referencing a
    /// parameter.
    pub locals: Vec<ValType>,

    /// The body is an instruction sequence that upon termination must produce an
    /// stack matching the function type's result type.
    pub body: Expr,
}

/// The globals component of a module defines a vector of global variables (or globals for
/// short): Each global stores a single value of the given global type. Its type also
/// specifies whether a global is immutable or mutable. Moreover, each global is initialized
/// with an init value given by a constant initializer expression. Globals are referenced
/// through global indices, starting with the smallest index not referencing a global import.
///
/// <https://www.w3.org/TR/wasm-core-2/#globals>
/// <https://www.w3.org/TR/wasm-core-2/#global-section>
#[derive(Debug, PartialEq)]
pub struct Global {
    pub r#type: GlobalType,
    pub init: Expr,
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

/// The initial contents of a table is uninitialized. Element segments can be used to
/// initialize a subrange of a table from a static vector of elements. The elems component
/// of a module defines a vector of element segments. Each element segment defines a
/// reference type and a corresponding list of constant element expressions. Element
/// segments have a mode that identifies them as either passive, active, or declarative. A
/// passive element segment's elements can be copied to a table using the table.init
/// instruction. An active element segment copies its elements into a table during
/// instantiation, as specified by a table index and a constant expression defining an
/// offset into that table. A declarative element segment is not available at runtime but
/// merely serves to forward-declare references that are formed in code with instructions
/// like ref.func. The offset is given by a constant expression. Element segments are
/// referenced through element indices.
///
/// <https://www.w3.org/TR/wasm-core-2/#element-segments>
/// <https://www.w3.org/TR/wasm-core-2/#element-section>
#[derive(Debug, PartialEq)]
pub struct Elem {
    pub r#type: RefType,
    pub init: Vec<Expr>,
    pub mode: ElemMode,
}

#[derive(Debug, PartialEq)]
pub enum ElemMode {
    Passive,
    Active { table: TableIdx, offset: Expr },
    Declare,
}
