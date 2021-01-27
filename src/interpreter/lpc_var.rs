use std::ops::{Add, Sub, Mul, Div};
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Copy, Clone)]
/// Represent a variable stored in a `Register`. `Int`s store the actual value.
/// Other types store an index into a `ConstantPool`.
/// This enum should remain `Copy`.
pub enum LPCVar {
    Int(i64),
    String(usize)
}

impl Display for LPCVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LPCVar::Int(x) => write!(f, "{}", x),
            LPCVar::String(x) => write!(f, "string with index {}", x)
        }
    }
}

impl Add for LPCVar {
    type Output = LPCVar;

    /// # Panics
    /// This will panic for any type other than LPCVar::Int
    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LPCVar::Int(x) => {
                match rhs {
                    LPCVar::Int(y) => LPCVar::Int(x + y),
                    _ => panic!("Unable to add these LPCVars directly, as they only \
                                contain indices into a ConstantPool. \
                                Resolve them to LPCConstants first.")
                }
            },
            _ => panic!("Unable to add these LPCVars directly, as they only \
                        contain indices into a ConstantPool. Resolve them to LPCConstants first.")
        }
    }
}

impl Sub for LPCVar {
    type Output = LPCVar;

    /// # Panics
    /// This will panic for any type other than LPCVar::Int
    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            LPCVar::Int(x) => {
                match rhs {
                    LPCVar::Int(y) => LPCVar::Int(x - y),
                    _ => panic!("Unable to subtract these LPCVars directly, as they only \
                                contain indices into a ConstantPool. \
                                Resolve them to LPCConstants first.")
                }
            },
            _ => panic!("Unable to subtract these LPCVars directly, as they only \
                        contain indices into a ConstantPool. \
                        Resolve them to LPCConstants first.")
        }
    }
}

impl Mul for LPCVar {
    type Output = LPCVar;

    /// # Panics
    /// This will panic for any type other than LPCVar::Int
    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            LPCVar::Int(x) => {
                match rhs {
                    LPCVar::Int(y) => LPCVar::Int(x * y),
                    _ => panic!("Unable to multiply these LPCVars directly, as they only \
                                contain indices into a ConstantPool. \
                                Resolve them to LPCConstants first.")
                }
            },
            _ => panic!("Unable to multiply these LPCVars directly, as they only \
                        contain indices into a ConstantPool. \
                        Resolve them to LPCConstants first.")
        }
    }
}

impl Div for LPCVar {
    type Output = LPCVar;

    /// # Panics
    /// This will panic for any type other than LPCVar::Int
    fn div(self, rhs: Self) -> Self::Output {
        match self {
            LPCVar::Int(x) => {
                match rhs {
                    LPCVar::Int(y) => LPCVar::Int(x / y),
                    _ => panic!("Unable to divide these LPCVars directly, as they only \
                                contain indices into a ConstantPool. \
                                Resolve them to LPCConstants first.")
                }
            },
            _ => panic!("Unable to divide these LPCVars directly, as they only \
                        contain indices into a ConstantPool. \
                        Resolve them to LPCConstants first.")
        }
    }
}
