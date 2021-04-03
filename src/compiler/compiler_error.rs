use codespan_reporting::diagnostic::Diagnostic;

use crate::errors::{NewError};
use std::{
    error::Error,
    fmt::{Display, Formatter},
};

/// General error wrapper type for the compiler
#[derive(Debug, Clone)]
pub enum CompilerError {
    NewError(NewError),
    Collection(Vec<CompilerError>),
}

impl CompilerError {
    /// Get the error diagnostics for printing to the user.
    pub fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        match self {
            CompilerError::Collection(errs) => {
                errs.iter().flat_map(|e| e.to_diagnostics()).collect()
            }
            CompilerError::NewError(err) => err.to_diagnostics(),
        }
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::Collection(errs) => {
                let s = errs
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "{}", s)
            }
            CompilerError::NewError(err) => write!(f, "{}", err),
        }
    }
}

impl Error for CompilerError {}
