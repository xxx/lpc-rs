use crate::errors::runtime_error::binary_operation_error::BinaryOperationError;
use codespan_reporting::diagnostic::Diagnostic;
use crate::errors::LPCError;
use crate::errors::runtime_error::division_by_zero_error::DivisionByZeroError;
use crate::errors::runtime_error::unknown_error::UnknownError;

pub mod binary_operation_error;
pub mod division_by_zero_error;
pub mod unknown_error;

#[derive(Debug)]
pub enum RuntimeError {
    BinaryOperationError(BinaryOperationError),
    DivisionByZeroError(DivisionByZeroError),
    UnknownError(UnknownError)
}

impl LPCError for RuntimeError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        match self {
            RuntimeError::BinaryOperationError(err) => err.to_diagnostics(file_id),
            RuntimeError::DivisionByZeroError(err) => err.to_diagnostics(file_id),
            RuntimeError::UnknownError(err) => err.to_diagnostics(file_id)
        }
    }
}
