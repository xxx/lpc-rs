use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fmt;

/// Handle preprocessing

#[derive(Debug)]
pub struct PreprocessorError(pub String);

impl Error for PreprocessorError {}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PreprocessorError: {}", self.0)
    }
}
