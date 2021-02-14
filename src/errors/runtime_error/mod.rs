use crate::errors::{
    runtime_error::{
        binary_operation_error::BinaryOperationError, division_by_zero_error::DivisionByZeroError,
        unknown_error::UnknownError,
    },
    LPCError,
};
use codespan_reporting::diagnostic::Diagnostic;
use crate::errors::runtime_error::index_error::IndexError;

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

impl LPCError for RuntimeError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        match self {
            RuntimeError::BinaryOperationError(err) => err.to_diagnostics(file_id),
            RuntimeError::DivisionByZeroError(err) => err.to_diagnostics(file_id),
            RuntimeError::IndexError(err) => err.to_diagnostics(file_id),
            RuntimeError::UnknownError(err) => err.to_diagnostics(file_id),
        }
    }
}
