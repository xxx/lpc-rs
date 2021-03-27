use crate::errors::{
    runtime_error::{
        binary_operation_error::BinaryOperationError, division_by_zero_error::DivisionByZeroError,
        index_error::IndexError, unknown_error::UnknownError,
    },
    LpcError,
};
use codespan_reporting::diagnostic::Diagnostic;

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
