use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

/// Handle preprocessing

#[derive(Debug)]
pub struct PreprocessorError(pub String);

impl Error for PreprocessorError {}

impl PreprocessorError {
    pub fn new(e: &str) -> Self {
        Self(String::from(e))
    }
}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PreprocessorError: {}", self.0)
    }
}
