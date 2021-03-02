use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

/// Handle preprocessing

#[derive(Debug)]
pub struct PreprocessorError(pub String);

impl Error for PreprocessorError {}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PreprocessorError: {}", self.0)
    }
}
