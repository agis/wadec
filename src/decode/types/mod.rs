pub mod functype;
pub use functype::DecodeFuncTypeError;

pub mod globaltype;
pub use globaltype::{DecodeGlobalTypeError, InvalidMutabilityByteError};

pub mod limits;
pub use limits::ParseLimitsError;

pub mod memtype;
pub use memtype::DecodeMemoryTypeError;

pub mod reftype;
pub use reftype::{DecodeRefTypeError, InvalidRefTypeMarkerError};

pub mod resulttype;
pub use resulttype::DecodeResultTypeError;

pub mod tabletype;
pub use tabletype::DecodeTableError;

pub mod valtype;
pub use valtype::{DecodeValTypeError, InvalidValTypeMarkerError};

pub mod tagtype;
pub use tagtype::DecodeTagTypeError;
