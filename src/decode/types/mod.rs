pub mod functype;
pub mod globaltype;
pub mod limits;
pub mod memtype;
pub mod reftype;
pub mod resulttype;
pub mod tabletype;
pub mod valtype;

pub use functype::DecodeFuncTypeError;
pub use globaltype::{DecodeGlobalTypeError, InvalidMutabilityByteError};
pub use limits::ParseLimitsError;
pub use memtype::DecodeMemoryTypeError;
pub use reftype::{DecodeRefTypeError, InvalidRefTypeMarkerError};
pub use resulttype::DecodeResultTypeError;
pub use tabletype::DecodeTableError;
pub use valtype::{DecodeValTypeError, InvalidValTypeMarkerError};
