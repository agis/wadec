use super::types::{functype::FuncType, memtype::MemType, tabletype::TableType};
use crate::decode::sections::{
    custom::CustomSection, data::Data, element::Elem, export::Export, function::Func,
    global::Global, import::Import,
};
use crate::indices::FuncIdx;

/// WebAssembly programs are organized into modules, which are the unit of deployment,
/// loading, and compilation. A module collects definitions for types, functions, tables,
/// memories, and globals. In addition, it can declare imports and exports and provide
/// initialization in the form of data and element segments, or a start function.
///
/// <https://www.w3.org/TR/wasm-core-2/#modules>
/// <https://www.w3.org/TR/wasm-core-2/#binary-module>
#[derive(Debug, PartialEq)]
pub struct Module {
    pub version: [u8; 4],

    // Contains any non-custom sections encountered.
    pub parsed_section_kinds: Vec<SectionKind>,

    pub section_headers: Vec<SectionHeader>,

    /// Custom sections have the id 0. They are intended to be used for debugging information or
    /// third-party extensions, and are ignored by the WebAssembly semantics. Their contents
    /// consist of a name further identifying the custom section, followed by an uninterpreted
    /// sequence of bytes for custom use.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#custom-section>
    /// <https://www.w3.org/TR/wasm-core-2/#binary-customsec>
    pub custom_sections: Vec<CustomSection>,

    /// The types component of a module defines a vector of function types. All function types
    /// used in a module must be defined in this component. They are referenced by type indices.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#types>
    /// <https://www.w3.org/TR/wasm-core-2/#binary-typesec>
    pub types: Vec<FuncType>,

    /// The funcs component of a module defines a vector of functions with the following
    /// structure: The type of a function declares its signature by reference to a type defined
    /// in the module. The parameters of the function are referenced through 0-based local
    /// indices in the function's body; they are mutable. The locals declare a vector of mutable
    /// local variables and their types. These variables are referenced through local indices in
    /// the function's body. The index of the first local is the smallest index not referencing a
    /// parameter. The body is an instruction sequence that upon termination must produce a
    /// stack matching the function type's result type. Functions are referenced through
    /// function indices, starting with the smallest index not referencing a function import.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#functions>
    /// <https://www.w3.org/TR/wasm-core-2/#function-section>
    pub funcs: Vec<Func>,

    /// The tables component of a module defines a vector of tables described by their table
    /// type: A table is a vector of opaque values of a particular reference type. The min size
    /// in the limits of the table type specifies the initial size of that table, while its max,
    /// if present, restricts the size to which it can grow later. Tables can be initialized
    /// through element segments. Tables are referenced through table indices, starting with the
    /// smallest index not referencing a table import. Most constructs implicitly reference
    /// table index 0.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#tables>
    /// <https://www.w3.org/TR/wasm-core-2/#table-section>
    pub tables: Vec<TableType>,

    /// The mems component of a module defines a vector of linear memories (or memories for
    /// short) as described by their memory type: A memory is a vector of raw uninterpreted
    /// bytes. The min size in the limits of the memory type specifies the initial size of that
    /// memory, while its max, if present, restricts the size to which it can grow later. Both
    /// are in units of page size. Memories can be initialized through data segments. Memories
    /// are referenced through memory indices, starting with the smallest index not referencing
    /// a memory import. Most constructs implicitly reference memory index 0.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#memories>
    /// <https://www.w3.org/TR/wasm-core-2/#memory-section>
    pub mems: Vec<MemType>,

    /// The globals component of a module defines a vector of global variables (or globals for
    /// short): Each global stores a single value of the given global type. Its type also
    /// specifies whether a global is immutable or mutable. Moreover, each global is initialized
    /// with an init value given by a constant initializer expression. Globals are referenced
    /// through global indices, starting with the smallest index not referencing a global import.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#globals>
    /// <https://www.w3.org/TR/wasm-core-2/#global-section>
    pub globals: Vec<Global>,

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
    pub elems: Vec<Elem>,

    /// The optional data count section (id 12) declares the number of data segments that follow.
    /// It is required when data indices appear in code, and when present its count must match the
    /// length of [`Self::datas`].
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#data-count-section>
    pub data_count: Option<u32>,

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
    pub datas: Vec<Data>,

    /// The start component of a module declares the function index of a start function that is
    /// automatically invoked when the module is instantiated, after tables and memories have
    /// been initialized.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#start-function>
    pub start: Option<FuncIdx>,

    /// The imports component of a module defines a set of imports that are required for
    /// instantiation. Each import is labeled by a two-level name space, consisting of a module
    /// name and a name for an entity within that module. Importable definitions are functions,
    /// tables, memories, and globals. Each import is specified by a descriptor with a
    /// respective type that a definition provided during instantiation is required to match.
    /// Every import defines an index in the respective index space. In each index space, the
    /// indices of imports go before the first index of any definition contained in the module
    /// itself.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#imports>
    /// <https://www.w3.org/TR/wasm-core-2/#import-section>
    pub imports: Vec<Import>,

    /// The exports component of a module defines a set of exports that become accessible to the
    /// host environment once the module has been instantiated. Each export is labeled by a
    /// unique name. Exportable definitions are functions, tables, memories, and globals, which
    /// are referenced through a respective descriptor.
    ///
    /// <https://www.w3.org/TR/wasm-core-2/#exports>
    /// <https://www.w3.org/TR/wasm-core-2/#export-section>
    pub exports: Vec<Export>,
}

/// Each section constists of a one-byte section id, the u32 size of the contents
/// (in bytes), and the actual contents, whose structure is dependent on the section id.
///
/// Note: 'Section header' is not a term defined in the specification.
#[derive(PartialEq, Debug)]
pub struct SectionHeader {
    pub kind: SectionKind,
    pub size: u32,
}

#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub enum SectionKind {
    Custom,
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    DataCount,
    Code,
    Data,
}
