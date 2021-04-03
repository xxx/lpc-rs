use crate::errors::{runtime_error::{
    index_error::IndexError, unknown_error::UnknownError,
}, LpcError, NewError};
use codespan_reporting::diagnostic::Diagnostic;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::{error::Error, fmt::Display};

pub mod index_error;
pub mod unknown_error;

#[derive(Debug)]
pub enum RuntimeError {
    IndexError(IndexError),
    UnknownError(UnknownError),

    NewError(NewError),
}

impl LpcError for RuntimeError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        match self {
            RuntimeError::IndexError(err) => err.to_diagnostics(),
            RuntimeError::UnknownError(err) => err.to_diagnostics(),
            RuntimeError::NewError(err) => err.to_diagnostics(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::IndexError(e) => write!(f, "{}", e),
            RuntimeError::UnknownError(e) => write!(f, "{}", e),
            RuntimeError::NewError(e) => write!(f, "{}", e),
        }
    }
}

impl Error for RuntimeError {}
