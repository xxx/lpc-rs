use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Sub, Mul, Div};
use crate::errors::runtime_error::RuntimeError;
use crate::errors::runtime_error::binary_operation_error::BinaryOperationError;
use crate::ast::binary_op_node::BinaryOperation;
use crate::errors::runtime_error::division_by_zero_error::DivisionByZeroError;

/// Represent a variable stored in a `Register`. `Int`s store the actual value.
/// Other types store an index into a `ConstantPool`.
/// This enum should remain `Copy`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LPCVar {
    Int(i64),
    String(usize),
    Array(usize),
}

impl LPCVar {
    pub fn type_name(&self) -> &str {
        match self {
            LPCVar::Int(_) => "int",
            LPCVar::String(_) => "string",
            LPCVar::Array(_) => "array",
        }
    }

    fn to_binary_op_error(
        &self,
        op: BinaryOperation,
        right: &LPCVar
    ) -> RuntimeError {
        let e = BinaryOperationError {
            op,
            left_type: self.type_name().to_string(),
            right_type: right.type_name().to_string(),
            span: None
        };

        RuntimeError::BinaryOperationError(e)
    }
}

impl Display for LPCVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LPCVar::Int(x) => write!(f, "{}", x),
            LPCVar::String(x) => write!(f, "string with index {}", x),
            LPCVar::Array(x) => write!(f, "array with index {}", x),
        }
    }
}

impl Add for LPCVar {
    type Output = Result<LPCVar, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        if let (LPCVar::Int(x), LPCVar::Int(y)) = (self, rhs) {
            Ok(LPCVar::Int(x + y))
        } else {
            Err(self.to_binary_op_error(BinaryOperation::Add, &rhs))
        }
    }
}

impl Sub for LPCVar {
    type Output = Result<LPCVar, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (LPCVar::Int(x), LPCVar::Int(y)) = (self, rhs) {
            Ok(LPCVar::Int(x - y))
        } else {
            Err(self.to_binary_op_error(BinaryOperation::Sub, &rhs))
        }
    }
}

impl Mul for LPCVar {
    type Output = Result<LPCVar, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (LPCVar::Int(x), LPCVar::Int(y)) = (self, rhs) {
            Ok(LPCVar::Int(x * y))
        } else {
            Err(self.to_binary_op_error(BinaryOperation::Mul, &rhs))
        }
    }
}

impl Div for LPCVar {
    type Output = Result<LPCVar, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        if let (LPCVar::Int(x), LPCVar::Int(y)) = (self, rhs) {
            if y == 0 {
                Err(RuntimeError::DivisionByZeroError(DivisionByZeroError { span: None }))
            } else {
                Ok(LPCVar::Int(x / y))
            }
        } else {
            Err(self.to_binary_op_error(BinaryOperation::Div, &rhs))
        }
    }
}
