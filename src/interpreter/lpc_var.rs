use crate::{ast::binary_op_node::BinaryOperation, errors::LpcError, LpcFloat, LpcInt};
use std::{
    fmt,
    fmt::{Display, Formatter},
    ops::{Add, Div, Mul, Sub},
};

/// Represent a variable stored in a `Register`. `Copy` types store the actual value.
/// Non-`Copy` types store an index into memory (i.e. an address).
/// This enum should remain `Copy`.
#[derive(Hash, Eq, Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub enum LpcRef {
    Float(LpcFloat),
    Int(LpcInt),
    String(usize),
    Array(usize),
    Mapping(usize),

    /// Stores an index into the program's `ConstantPool`, rather than memory.
    StringConstant(usize),
}

impl LpcRef {
    /// Get the type name of the underlying data of this var.
    pub fn type_name(&self) -> &str {
        match self {
            LpcRef::Float(_) => "float",
            LpcRef::Int(_) => "int",
            LpcRef::String(_) => "string",
            LpcRef::Array(_) => "array",
            LpcRef::Mapping(_) => "mapping",
            LpcRef::StringConstant(_) => "string",
        }
    }

    fn to_error(&self, op: BinaryOperation, right: &LpcRef) -> LpcError {
        LpcError::new(format!(
            "Runtime Error: Mismatched types: ({}) {} ({})",
            self.type_name(),
            op,
            right.type_name()
        ))
    }
}

impl Display for LpcRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcRef::Float(x) => write!(f, "{}", x),
            LpcRef::Int(x) => write!(f, "{}", x),
            LpcRef::String(x) => write!(f, "string with index {}", x),
            LpcRef::Array(x) => write!(f, "array with index {}", x),
            LpcRef::Mapping(x) => write!(f, "mapping with index {}", x),
            LpcRef::StringConstant(x) => write!(f, "string (constant) with index {}", x),
        }
    }
}

impl Add for LpcRef {
    type Output = Result<LpcRef, LpcError>;

    fn add(self, rhs: Self) -> Self::Output {
        if let (LpcRef::Int(x), LpcRef::Int(y)) = (self, rhs) {
            Ok(LpcRef::Int(x + y))
        } else if let (LpcRef::Float(x), LpcRef::Float(y)) = (self, rhs) {
            Ok(LpcRef::Float(x + y))
        } else if let (LpcRef::Float(x), LpcRef::Int(y)) = (self, rhs) {
            Ok(LpcRef::Float(x + y as f64))
        } else if let (LpcRef::Int(x), LpcRef::Float(y)) = (self, rhs) {
            Ok(LpcRef::Float(LpcFloat::from(x as f64) + y))
        } else {
            Err(self.to_error(BinaryOperation::Add, &rhs))
        }
    }
}

impl Sub for LpcRef {
    type Output = Result<LpcRef, LpcError>;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (LpcRef::Int(x), LpcRef::Int(y)) = (self, rhs) {
            Ok(LpcRef::Int(x - y))
        } else if let (LpcRef::Float(x), LpcRef::Float(y)) = (self, rhs) {
            Ok(LpcRef::Float(x - y))
        } else if let (LpcRef::Float(x), LpcRef::Int(y)) = (self, rhs) {
            Ok(LpcRef::Float(x - y as f64))
        } else if let (LpcRef::Int(x), LpcRef::Float(y)) = (self, rhs) {
            Ok(LpcRef::Float(LpcFloat::from(x as f64) - y))
        } else {
            Err(self.to_error(BinaryOperation::Sub, &rhs))
        }
    }
}

impl Mul for LpcRef {
    type Output = Result<LpcRef, LpcError>;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (LpcRef::Int(x), LpcRef::Int(y)) = (self, rhs) {
            Ok(LpcRef::Int(x * y))
        } else if let (LpcRef::Float(x), LpcRef::Float(y)) = (self, rhs) {
            Ok(LpcRef::Float(x * y))
        } else if let (LpcRef::Float(x), LpcRef::Int(y)) = (self, rhs) {
            Ok(LpcRef::Float(x * y as f64))
        } else if let (LpcRef::Int(x), LpcRef::Float(y)) = (self, rhs) {
            Ok(LpcRef::Float(LpcFloat::from(x as f64) * y))
        } else {
            Err(self.to_error(BinaryOperation::Mul, &rhs))
        }
    }
}

impl Div for LpcRef {
    type Output = Result<LpcRef, LpcError>;

    fn div(self, rhs: Self) -> Self::Output {
        if let (LpcRef::Int(x), LpcRef::Int(y)) = (self, rhs) {
            if y == 0 {
                Err(LpcError::new("Runtime Error: Division by zero"))
            } else {
                Ok(LpcRef::Int(x / y))
            }
        } else if let (LpcRef::Float(x), LpcRef::Float(y)) = (self, rhs) {
            if y == 0.0 {
                Err(LpcError::new("Runtime Error: Division by zero"))
            } else {
                Ok(LpcRef::Float(x / y))
            }
        } else if let (LpcRef::Float(x), LpcRef::Int(y)) = (self, rhs) {
            if y == 0 {
                Err(LpcError::new("Runtime Error: Division by zero"))
            } else {
                Ok(LpcRef::Float(x / y as f64))
            }
        } else if let (LpcRef::Int(x), LpcRef::Float(y)) = (self, rhs) {
            if y == 0.0 {
                Err(LpcError::new("Runtime Error: Division by zero"))
            } else {
                Ok(LpcRef::Float(LpcFloat::from(x as f64) / y))
            }
        } else {
            Err(self.to_error(BinaryOperation::Div, &rhs))
        }
    }
}
