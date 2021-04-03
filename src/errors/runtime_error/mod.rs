use crate::errors::{
    runtime_error::{
        binary_operation_error::BinaryOperationError, division_by_zero_error::DivisionByZeroError,
        index_error::IndexError, unknown_error::UnknownError,
    },
    LpcError,
};
use codespan_reporting::diagnostic::Diagnostic;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::{error::Error, fmt::Display};

pub mod binary_operation_error;
pub mod division_by_zero_error;
pub mod index_error;
pub mod unknown_error;

#[derive(Debug)]
pub enum RuntimeError {
    BinaryOperationError(BinaryOperationError),
    DivisionByZeroError(DivisionByZeroError),
    IndexError(IndexError),
    UnknownError(UnknownError),
}

impl LpcError for RuntimeError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        match self {
            RuntimeError::BinaryOperationError(err) => err.to_diagnostics(),
            RuntimeError::DivisionByZeroError(err) => err.to_diagnostics(),
            RuntimeError::IndexError(err) => err.to_diagnostics(),
            RuntimeError::UnknownError(err) => err.to_diagnostics(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::BinaryOperationError(e) => write!(f, "{}", e),
            RuntimeError::DivisionByZeroError(e) => write!(f, "{}", e),
            RuntimeError::IndexError(e) => write!(f, "{}", e),
            RuntimeError::UnknownError(e) => write!(f, "{}", e),
        }
    }
}

impl Error for RuntimeError {}
