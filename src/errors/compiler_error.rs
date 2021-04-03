use codespan_reporting::diagnostic::Diagnostic;

use crate::errors::{NewError};
use std::{
    error::Error,
    fmt::{Display, Formatter},
};

/// General error wrapper type for the compiler
#[derive(Debug, Clone)]
pub enum LpcError {
    MultiError(Vec<LpcError>),

    NewError(NewError),
}

impl LpcError {
    /// Get the error diagnostics for printing to the user.
    pub fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        match self {
            LpcError::MultiError(errs) => {
                errs.iter().flat_map(|e| e.to_diagnostics()).collect()
            }
            LpcError::NewError(err) => err.to_diagnostics(),
        }
    }
}

impl Display for LpcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LpcError::MultiError(errs) => {
                let s = errs
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "{}", s)
            }
            LpcError::NewError(err) => write!(f, "{}", err),
        }
    }
}

impl Error for LpcError {}
