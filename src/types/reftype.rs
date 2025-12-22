/// Reference types classify first-class references to objects in the runtime store.
///
/// The type funcref denotes the infinite union of all references to functions, regardless of
/// their function types.
///
/// The type externref denotes the infinite union of all references to
/// objects owned by the embedder and that can be passed into WebAssembly under this type.
///
/// Reference types are opaque, meaning that neither their size nor their bit pattern can be
/// observed. Values of reference type can be stored in tables.
///
/// <https://www.w3.org/TR/wasm-core-2/#reference-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-reftype>
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum RefType {
    Func,
    Extern,
}
