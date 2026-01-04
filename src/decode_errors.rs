//! Convenience re-exports for decode-related error types.

pub use crate::decode::indices::{
    DecodeDataIdxError, DecodeElemIdxError, DecodeFuncIdxError, DecodeGlobalIdxError,
    DecodeLabelIdxError, DecodeLocalIdxError, DecodeMemIdxError, DecodeTableIdxError,
    DecodeTypeIdxError,
};
pub use crate::decode::instructions::{
    BlockTypeError, ControlError, LaneIdxError, MemargError, MemoryError, NumericError,
    ParametricError, ParseError, ReferenceError, TableError, VariableError, VectorError,
};
pub use crate::decode::integer::{DecodeI32Error, DecodeI64Error, DecodeU32Error, DecodeU64Error};
pub use crate::decode::sections::code::{DecodeCodeError, DecodeCodeLocalsError};
pub use crate::decode::sections::data::DecodeDataSegmentError;
pub use crate::decode::sections::element::{DecodeElementError, DecodeElementKindError};
pub use crate::decode::sections::export::DecodeExportError;
pub use crate::decode::sections::global::DecodeGlobalError;
pub use crate::decode::sections::import::DecodeImportError;
pub use crate::decode::sections::{
    DecodeCodeSectionError, DecodeCustomSectionError, DecodeDataCountSectionError,
    DecodeDataSectionError, DecodeElementSectionError, DecodeExportSectionError,
    DecodeFunctionSectionError, DecodeGlobalSectionError, DecodeImportSectionError,
    DecodeMemorySectionError, DecodeStartSectionError, DecodeTableSectionError,
    DecodeTypeSectionError,
};
pub use crate::decode::types::{
    DecodeFuncTypeError, DecodeGlobalTypeError, DecodeMemoryTypeError, DecodeRefTypeError,
    DecodeResultTypeError, DecodeTableTypeError, DecodeValTypeError, InvalidMutabilityByteError,
    InvalidRefTypeMarkerError, InvalidValTypeMarkerError, ParseLimitsError,
};
pub use crate::decode::{
    DecodeByteVectorError, DecodeFloat32Error, DecodeFloat64Error, DecodeListError,
    DecodeModuleError, DecodeNameError, DecodeSectionHeaderError, InvalidSectionIdError,
    ParseExpressionError, ParsePreambleError,
};
