use crate::errors::{LpcError, NewError};
use codespan_reporting::diagnostic::Diagnostic;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum RuntimeError {
    NewError(NewError),
}

impl LpcError for RuntimeError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        match self {
            RuntimeError::NewError(err) => err.to_diagnostics(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::NewError(e) => write!(f, "{}", e),
        }
    }
}

impl Error for RuntimeError {}
