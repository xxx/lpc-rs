use crate::{errors::lazy_files::FileId, parser::span::Span};

/// Some small wrappers to store both a file id and a value for compatibility with Logos,
/// which only allows a single field in token defs.
#[derive(Debug, Clone, PartialEq)]
pub struct FloatToken(pub Span, pub f64);

impl FloatToken {
    pub fn file_id(&self) -> FileId {
        self.0.file_id
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntToken(pub Span, pub i64);

impl IntToken {
    pub fn file_id(&self) -> FileId {
        self.0.file_id
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringToken(pub Span, pub String);

impl StringToken {
    pub fn file_id(&self) -> FileId {
        self.0.file_id
    }
}
