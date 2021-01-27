use std::ops::{Add, Sub, Mul, Div};
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Copy, Clone)]
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

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LPCVar::Int(x) => {
                match rhs {
                    LPCVar::Int(y) => LPCVar::Int(x + y),
                    _ => unimplemented!()
                }
            },
            _ => unimplemented!()
        }
    }
}

impl Sub for LPCVar {
    type Output = LPCVar;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            LPCVar::Int(x) => {
                match rhs {
                    LPCVar::Int(y) => LPCVar::Int(x - y),
                    _ => unimplemented!()
                }
            },
            _ => unimplemented!()
        }
    }
}

impl Mul for LPCVar {
    type Output = LPCVar;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            LPCVar::Int(x) => {
                match rhs {
                    LPCVar::Int(y) => LPCVar::Int(x * y),
                    _ => unimplemented!()
                }
            },
            _ => unimplemented!()
        }
    }
}

impl Div for LPCVar {
    type Output = LPCVar;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            LPCVar::Int(x) => {
                match rhs {
                    LPCVar::Int(y) => LPCVar::Int(x / y),
                    _ => unimplemented!()
                }
            },
            _ => unimplemented!()
        }
    }
}
