use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};
use std::path::PathBuf;

/// Handle preprocessing

#[derive(Debug)]
pub struct PreprocessorError {
    pub message: String,
    pub line_num: usize,
    pub filename: PathBuf
}

impl Error for PreprocessorError {}

impl PreprocessorError {
    pub fn new(e: &str, line_num: usize, f: &str) -> Self {
        Self {
            message: String::from(e),
            line_num,
            filename: PathBuf::from(f)
        }
    }
}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PreprocessorError: {}", self.message)
    }
}
