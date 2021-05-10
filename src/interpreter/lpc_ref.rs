use crate::{
    ast::binary_op_node::BinaryOperation, errors::LpcError, interpreter::lpc_value::LpcValue,
    LpcFloat, LpcInt,
};
use fasthash::metro;
use refpool::PoolRef;
use std::{
    cell::RefCell,
    fmt,
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    ops::{Add, Div, Mul, Sub},
    ptr,
};

type StringHash = u128;

/// Represent a variable stored in a `Register`. Value types store the actual value.
/// Reference types store a reference to the actual value.
#[derive(Eq, Debug, Clone)]
pub enum LpcRef {
    Float(LpcFloat),
    Int(LpcInt),

    /// Reference type, and stores a reference-counting pointer to the actual value
    String(PoolRef<RefCell<LpcValue>>),

    /// Reference type, and stores a reference-counting pointer to the actual value
    Array(PoolRef<RefCell<LpcValue>>),

    /// Reference type, and stores a reference-counting pointer to the actual value
    Mapping(PoolRef<RefCell<LpcValue>>),

    /// Stores an index into a [`Program`]'s [`ConstantPool`], rather than a memory reference.
    StringConstant(usize),

    /// A reference that contains a string's hash value, used only for keys in mappings.
    StringHash(StringHash),
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
            LpcRef::StringHash(_) => "string",
        }
    }

    /// A helper to use our specified hasher to create [`StringHash`]es.
    pub fn hash_string(s: &str) -> StringHash {
        metro::hash128(s)
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

macro_rules! extract_value {
    ( $x:expr, $y:path ) => {
        match &$x {
            $y(s) => s,
            x => panic!("Invalid LpcValue - received `{}`. This indicates a serious bug in the interpreter.", x)
        }
    };
}

impl Hash for LpcRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LpcRef::Float(f) => f.hash(state),
            LpcRef::Int(i) => i.hash(state),
            LpcRef::String(s) => extract_value!(*s.borrow(), LpcValue::String).hash(state),
            LpcRef::Array(a) => {
                ptr::hash(&**a, state)
                // extract_value!(*a.borrow(), LpcValue::Array).hash(state)
            }
            LpcRef::Mapping(m) => {
                ptr::hash(&**m, state)
                // extract_value!(*m.borrow(), LpcValue::Mapping).hash(state)
            }
            LpcRef::StringConstant(s) => s.hash(state),
            LpcRef::StringHash(sh) => sh.hash(state),
        }
    }
}

impl PartialEq for LpcRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LpcRef::Float(x), LpcRef::Float(y)) => x == y,
            (LpcRef::Int(x), LpcRef::Int(y)) => x == y,
            (LpcRef::String(x), LpcRef::String(y)) => {
                extract_value!(*x.borrow(), LpcValue::String)
                    == extract_value!(*y.borrow(), LpcValue::String)
            }
            (LpcRef::Array(x), LpcRef::Array(y)) | (LpcRef::Mapping(x), LpcRef::Mapping(y)) => {
                ptr::eq(x.as_ref(), y.as_ref())
            }
            (LpcRef::StringConstant(_x), LpcRef::StringConstant(_y)) => todo!(),
            (LpcRef::String(_x), LpcRef::StringConstant(_y)) => todo!(),
            (LpcRef::StringConstant(_x), LpcRef::String(_y)) => todo!(),
            (LpcRef::StringHash(x), LpcRef::StringHash(y)) => x == y,
            _ => false,
        }
    }
}

impl Display for LpcRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcRef::Float(x) => write!(f, "{}", x),
            LpcRef::Int(x) => write!(f, "{}", x),
            LpcRef::String(x) => write!(f, "string with index {}", x.borrow()),
            LpcRef::Array(x) => write!(f, "array with index {}", x.borrow()),
            LpcRef::Mapping(x) => write!(f, "mapping with index {}", x.borrow()),
            LpcRef::StringConstant(x) => write!(f, "string (constant) with index {}", x),
            LpcRef::StringHash(hash) => write!(f, "string with hash {}", hash),
        }
    }
}

impl Add for LpcRef {
    type Output = Result<LpcRef, LpcError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcRef::Int(x + y)),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(LpcRef::Float(*x + *y)),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(LpcRef::Float(*x + *y as f64)),
            (LpcRef::Int(x), LpcRef::Float(y)) => Ok(LpcRef::Float(LpcFloat::from(*x as f64) + *y)),
            _ => Err(self.to_error(BinaryOperation::Add, &rhs)),
        }
    }
}

impl Sub for LpcRef {
    type Output = Result<LpcRef, LpcError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcRef::Int(*x - *y)),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(LpcRef::Float(*x - *y)),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(LpcRef::Float(*x - *y as f64)),
            (LpcRef::Int(x), LpcRef::Float(y)) => Ok(LpcRef::Float(LpcFloat::from(*x as f64) - *y)),
            _ => Err(self.to_error(BinaryOperation::Sub, &rhs)),
        }
    }
}

impl Mul for LpcRef {
    type Output = Result<LpcRef, LpcError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcRef::Int(*x * *y)),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(LpcRef::Float(*x * *y)),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(LpcRef::Float(*x * *y as f64)),
            (LpcRef::Int(x), LpcRef::Float(y)) => Ok(LpcRef::Float(LpcFloat::from(*x as f64) * *y)),
            _ => Err(self.to_error(BinaryOperation::Mul, &rhs)),
        }
    }
}

impl Div for LpcRef {
    type Output = Result<LpcRef, LpcError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                if y == &0 {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcRef::Int(*x / *y))
                }
            }
            (LpcRef::Float(x), LpcRef::Float(y)) => {
                if (*y - LpcFloat::from(0.0)).into_inner().abs() < f64::EPSILON {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcRef::Float(*x / *y))
                }
            }
            (LpcRef::Float(x), LpcRef::Int(y)) => {
                if y == &0 {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcRef::Float(*x / *y as f64))
                }
            }
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                if (*y - LpcFloat::from(0.0)).into_inner().abs() < f64::EPSILON {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcRef::Float(LpcFloat::from(*x as f64) / *y))
                }
            }
            _ => Err(self.to_error(BinaryOperation::Div, &rhs)),
        }
    }
}