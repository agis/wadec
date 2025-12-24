/// Number types classify numeric values.
///
/// The types i32 and i64 classify 32 and 64 bit integers, respectively. Integers are not
/// inherently signed or unsigned, their interpretation is determined by individual operations.
///
/// The types f32 and f64 classify 32 and 64 bit floating-point data, respectively. They correspond to the respective binary
/// floating-point representations, also known as single and double precision, as defined by
/// the IEEE 754 standard (Section 3.3).
///
/// Number types are transparent, meaning that their bit patterns can be observed. Values of number
/// type can be stored in memories.
///
/// <https://www.w3.org/TR/wasm-core-2/#number-types>
/// <https://www.w3.org/TR/wasm-core-2/#binary-numtype>
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumType {
    Int32,
    Int64,
    Float32,
    Float64,
}
