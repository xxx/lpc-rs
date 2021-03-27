use crate::{
    ast::binary_op_node::BinaryOperation,
    errors::runtime_error::{
        binary_operation_error::BinaryOperationError, division_by_zero_error::DivisionByZeroError,
        RuntimeError,
    },
};
use std::{
    fmt,
    fmt::{Display, Formatter},
    ops::{Add, Div, Mul, Sub},
};

/// Represent a variable stored in a `Register`. `Copy` types store the actual value.
/// Non-`Copy` types store an index into memory (i.e. an address).
/// This enum should remain `Copy`.
#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub enum LpcVar {
    Float(f64),
    Int(i64),
    String(usize),
    Array(usize),

    /// Stores an index into the program's `ConstantPool`, rather than memory.
    StringConstant(usize),
}

impl LpcVar {
    /// Get the type name of the underlying data of this var.
    pub fn type_name(&self) -> &str {
        match self {
            LpcVar::Float(_) => "float",
            LpcVar::Int(_) => "int",
            LpcVar::String(_) => "string",
            LpcVar::Array(_) => "array",
            LpcVar::StringConstant(_) => "string",
        }
    }

    fn to_binary_op_error(&self, op: BinaryOperation, right: &LpcVar) -> RuntimeError {
        let e = BinaryOperationError {
            op,
            left_type: self.type_name().to_string(),
            right_type: right.type_name().to_string(),
            span: None,
        };

        RuntimeError::BinaryOperationError(e)
    }
}

impl Display for LpcVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcVar::Float(x) => write!(f, "{}", x),
            LpcVar::Int(x) => write!(f, "{}", x),
            LpcVar::String(x) => write!(f, "string with index {}", x),
            LpcVar::Array(x) => write!(f, "array with index {}", x),
            LpcVar::StringConstant(x) => write!(f, "string (constant) with index {}", x),
        }
    }
}

impl Add for LpcVar {
    type Output = Result<LpcVar, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        if let (LpcVar::Int(x), LpcVar::Int(y)) = (self, rhs) {
            Ok(LpcVar::Int(x + y))
        } else if let (LpcVar::Float(x), LpcVar::Float(y)) = (self, rhs) {
            Ok(LpcVar::Float(x + y))
        } else if let (LpcVar::Float(x), LpcVar::Int(y)) = (self, rhs) {
            Ok(LpcVar::Float(x + y as f64))
        } else if let (LpcVar::Int(x), LpcVar::Float(y)) = (self, rhs) {
            Ok(LpcVar::Float(x as f64 + y))
        } else {
            Err(self.to_binary_op_error(BinaryOperation::Add, &rhs))
        }
    }
}

impl Sub for LpcVar {
    type Output = Result<LpcVar, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (LpcVar::Int(x), LpcVar::Int(y)) = (self, rhs) {
            Ok(LpcVar::Int(x - y))
        } else if let (LpcVar::Float(x), LpcVar::Float(y)) = (self, rhs) {
            Ok(LpcVar::Float(x - y))
        } else if let (LpcVar::Float(x), LpcVar::Int(y)) = (self, rhs) {
            Ok(LpcVar::Float(x - y as f64))
        } else if let (LpcVar::Int(x), LpcVar::Float(y)) = (self, rhs) {
            Ok(LpcVar::Float(x as f64 - y))
        } else {
            Err(self.to_binary_op_error(BinaryOperation::Sub, &rhs))
        }
    }
}

impl Mul for LpcVar {
    type Output = Result<LpcVar, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (LpcVar::Int(x), LpcVar::Int(y)) = (self, rhs) {
            Ok(LpcVar::Int(x * y))
        } else if let (LpcVar::Float(x), LpcVar::Float(y)) = (self, rhs) {
            Ok(LpcVar::Float(x * y))
        } else if let (LpcVar::Float(x), LpcVar::Int(y)) = (self, rhs) {
            Ok(LpcVar::Float(x * y as f64))
        } else if let (LpcVar::Int(x), LpcVar::Float(y)) = (self, rhs) {
            Ok(LpcVar::Float(x as f64 * y))
        } else {
            Err(self.to_binary_op_error(BinaryOperation::Mul, &rhs))
        }
    }
}

impl Div for LpcVar {
    type Output = Result<LpcVar, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        if let (LpcVar::Int(x), LpcVar::Int(y)) = (self, rhs) {
            if y == 0 {
                Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                    span: None,
                }))
            } else {
                Ok(LpcVar::Int(x / y))
            }
        } else if let (LpcVar::Float(x), LpcVar::Float(y)) = (self, rhs) {
            if y == 0.0 {
                Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                    span: None,
                }))
            } else {
                Ok(LpcVar::Float(x / y))
            }
        } else if let (LpcVar::Float(x), LpcVar::Int(y)) = (self, rhs) {
            if y == 0 {
                Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                    span: None,
                }))
            } else {
                Ok(LpcVar::Float(x / y as f64))
            }
        } else if let (LpcVar::Int(x), LpcVar::Float(y)) = (self, rhs) {
            if y == 0.0 {
                Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                    span: None,
                }))
            } else {
                Ok(LpcVar::Float(x as f64 / y))
            }
        } else {
            Err(self.to_binary_op_error(BinaryOperation::Div, &rhs))
        }
    }
}
