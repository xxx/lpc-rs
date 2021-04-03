use crate::errors::{runtime_error::{unknown_error::UnknownError}, LpcError, NewError};
use codespan_reporting::diagnostic::Diagnostic;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::{error::Error, fmt::Display};

pub mod unknown_error;

#[derive(Debug)]
pub enum RuntimeError {
    UnknownError(UnknownError),

    NewError(NewError),
}

impl LpcError for RuntimeError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        match self {
            RuntimeError::UnknownError(err) => err.to_diagnostics(),
            RuntimeError::NewError(err) => err.to_diagnostics(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::UnknownError(e) => write!(f, "{}", e),
            RuntimeError::NewError(e) => write!(f, "{}", e),
        }
    }
}

impl Error for RuntimeError {}
