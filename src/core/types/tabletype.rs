use super::limits::Limits;
use super::reftype::RefType;
use crate::Expr;
use crate::core::indices::TableIdx;

/// Table types classify tables over elements of reference type within a size range. Like
/// memories, tables are constrained by limits for their minimum and optionally maximum
/// size. The limits are given in numbers of entries.
///
/// <https://www.w3.org/TR/wasm-core-2/#table-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-tabletype>
#[derive(Debug, PartialEq)]
pub struct TableType {
    pub reftype: RefType,
    pub limits: Limits,
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
    Declarative,
}
