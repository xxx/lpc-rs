use crate::errors::lazy_files::FileId;

/// Some small wrappers to store both a file id and a value for compatibility with Logos,
/// which only allows a single field in token defs.
#[derive(Debug, Clone, PartialEq)]
pub struct FloatToken(pub FileId, pub f64);
#[derive(Debug, Clone, PartialEq)]
pub struct IntToken(pub FileId, pub i64);
#[derive(Debug, Clone, PartialEq)]
pub struct StringToken(pub FileId, pub String);
