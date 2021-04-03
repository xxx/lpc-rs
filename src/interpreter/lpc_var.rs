use crate::{
    ast::binary_op_node::BinaryOperation,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
    ops::{Add, Div, Mul, Sub},
};
use crate::errors::LpcError;
use crate::compiler::compiler_error::CompilerError;

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

    fn to_error(&self, op: BinaryOperation, right: &LpcVar) -> CompilerError {
        let e = LpcError::new(format!("Runtime Error: Mismatched types: ({}) {} ({})", self.type_name(), op, right.type_name()));

        CompilerError::LpcError(e)
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
    type Output = Result<LpcVar, CompilerError>;

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
            Err(self.to_error(BinaryOperation::Add, &rhs))
        }
    }
}

impl Sub for LpcVar {
    type Output = Result<LpcVar, CompilerError>;

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
            Err(self.to_error(BinaryOperation::Sub, &rhs))
        }
    }
}

impl Mul for LpcVar {
    type Output = Result<LpcVar, CompilerError>;

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
            Err(self.to_error(BinaryOperation::Mul, &rhs))
        }
    }
}

impl Div for LpcVar {
    type Output = Result<LpcVar, CompilerError>;

    fn div(self, rhs: Self) -> Self::Output {
        if let (LpcVar::Int(x), LpcVar::Int(y)) = (self, rhs) {
            if y == 0 {
                Err(CompilerError::LpcError(LpcError::new("Runtime Error: Division by zero")))
            } else {
                Ok(LpcVar::Int(x / y))
            }
        } else if let (LpcVar::Float(x), LpcVar::Float(y)) = (self, rhs) {
            if y == 0.0 {
                Err(CompilerError::LpcError(LpcError::new("Runtime Error: Division by zero")))
            } else {
                Ok(LpcVar::Float(x / y))
            }
        } else if let (LpcVar::Float(x), LpcVar::Int(y)) = (self, rhs) {
            if y == 0 {
                Err(CompilerError::LpcError(LpcError::new("Runtime Error: Division by zero")))
            } else {
                Ok(LpcVar::Float(x / y as f64))
            }
        } else if let (LpcVar::Int(x), LpcVar::Float(y)) = (self, rhs) {
            if y == 0.0 {
                Err(CompilerError::LpcError(LpcError::new("Runtime Error: Division by zero")))
            } else {
                Ok(LpcVar::Float(x as f64 / y))
            }
        } else {
            Err(self.to_error(BinaryOperation::Div, &rhs))
        }
    }
}
