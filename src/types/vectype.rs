/// Vector types classify vectors of numeric values processed by vector instructions (also
/// known as SIMD instructions, single instruction multiple data).
///
/// The type v128 corresponds to a 128 bit vector of packed integer or floating-point data. The
/// packed data can be interpreted as signed or unsigned integers, single or double precision
/// floating-point values, or a single 128 bit type. The interpretation is determined by individual
/// operations.
///
/// Vector types, like number types are transparent, meaning that their bit patterns
/// can be observed. Values of vector type can be stored in memories.
///
/// <https://www.w3.org/TR/wasm-core-2/#vector-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-vectype>
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum VecType {
    V128,
}
