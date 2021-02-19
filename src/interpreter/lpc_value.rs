use crate::{
    ast::binary_op_node::BinaryOperation,
    errors::runtime_error::{
        binary_operation_error::BinaryOperationError, division_by_zero_error::DivisionByZeroError,
        RuntimeError,
    },
    interpreter::lpc_var::LPCVar,
};
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::{
    fmt,
    fmt::Display,
    iter::repeat,
    ops::{Add, Div, Mul, Sub},
};

/// An actual LPC value. These are stored in memory, and as constants.
/// They are only used in the interpreter.
#[derive(Debug, Clone, PartialEq)]
pub enum LPCValue {
    Float(f64),
    Int(i64),
    String(String),
    Array(Vec<LPCVar>),
}

impl LPCValue {
    pub fn type_name(&self) -> &str {
        match self {
            LPCValue::Float(_) => "float",
            LPCValue::Int(_) => "int",
            LPCValue::String(_) => "string",
            LPCValue::Array(_) => "array",
        }
    }

    // Just a refactor of a common operation
    fn to_binary_op_error(&self, op: BinaryOperation, right: &LPCValue) -> RuntimeError {
        let e = BinaryOperationError {
            op,
            left_type: self.type_name().to_string(),
            right_type: right.type_name().to_string(),
            span: None,
        };

        RuntimeError::BinaryOperationError(e)
    }
}

impl Display for LPCValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LPCValue::Float(fl) => write!(f, "{}", fl),
            LPCValue::Int(i) => write!(f, "{}", i),
            LPCValue::String(s) => write!(f, "{}", s),
            LPCValue::Array(a) => write!(f, "({{ {:?} }})", a),
        }
    }
}

impl From<&String> for LPCValue {
    fn from(s: &String) -> Self {
        Self::String(String::from(s))
    }
}

impl From<Vec<LPCVar>> for LPCValue {
    fn from(v: Vec<LPCVar>) -> Self {
        Self::Array(v)
    }
}

impl Add for &LPCValue {
    type Output = Result<LPCValue, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Float(f) => match rhs {
                LPCValue::Float(f2) => Ok(LPCValue::Float(f + f2)),
                LPCValue::Int(i) => Ok(LPCValue::Float(f + *i as f64)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Add, rhs)),
            },
            LPCValue::Int(i) => match rhs {
                LPCValue::Float(f) => Ok(LPCValue::Float(*i as f64 + f)),
                LPCValue::Int(i2) => Ok(LPCValue::Int(i + i2)),
                LPCValue::String(s) => Ok(LPCValue::String(i.to_string() + &s)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Add, rhs)),
            },
            LPCValue::String(s) => match rhs {
                LPCValue::String(s2) => Ok(LPCValue::String(s.clone() + s2)),
                LPCValue::Int(i) => Ok(LPCValue::String(s.clone() + &i.to_string())),
                _ => Err(self.to_binary_op_error(BinaryOperation::Add, rhs)),
            },
            LPCValue::Array(vec) => match rhs {
                LPCValue::Array(vec2) => {
                    let mut new_vec = vec.to_vec();
                    new_vec.extend(&*vec2);
                    Ok(LPCValue::Array(new_vec))
                }
                _ => Err(self.to_binary_op_error(BinaryOperation::Add, rhs)),
            },
        }
    }
}

impl Sub for &LPCValue {
    type Output = Result<LPCValue, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => match rhs {
                LPCValue::Float(f) => Ok(LPCValue::Float(*i as f64 - f)),
                LPCValue::Int(i2) => Ok(LPCValue::Int(i - i2)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Sub, rhs)),
            },
            LPCValue::Float(f) => match rhs {
                LPCValue::Float(f2) => Ok(LPCValue::Float(f - f2)),
                LPCValue::Int(i) => Ok(LPCValue::Float(f - *i as f64)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Sub, rhs)),
            },
            _ => Err(self.to_binary_op_error(BinaryOperation::Sub, rhs)),
        }
    }
}

/// Repeat `s`, `i` times, and return a new String of it.
fn repeat_string(s: &str, i: &i64) -> String {
    if *i >= 0 {
        repeat(s).take(*i as usize).collect::<String>()
    } else {
        String::from("")
    }
}

impl Mul for &LPCValue {
    type Output = Result<LPCValue, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Float(f) => match rhs {
                LPCValue::Float(f2) => Ok(LPCValue::Float(f * f2)),
                LPCValue::Int(i) => Ok(LPCValue::Float(f * *i as f64)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Mul, rhs)),
            },
            LPCValue::Int(i) => match rhs {
                LPCValue::Float(f) => Ok(LPCValue::Float(*i as f64 * f)),
                LPCValue::Int(i2) => Ok(LPCValue::Int(i * i2)),
                LPCValue::String(s) => Ok(LPCValue::String(repeat_string(s, i))),
                _ => Err(self.to_binary_op_error(BinaryOperation::Mul, rhs)),
            },
            LPCValue::String(s) => {
                match rhs {
                    // repeat the string `s`, `i` times
                    LPCValue::Int(i) => Ok(LPCValue::String(repeat_string(s, i))),
                    _ => Err(self.to_binary_op_error(BinaryOperation::Mul, rhs)),
                }
            }
            _ => Err(self.to_binary_op_error(BinaryOperation::Mul, rhs)),
        }
    }
}

impl Div for &LPCValue {
    type Output = Result<LPCValue, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Float(f) => match rhs {
                LPCValue::Float(f2) => {
                    if *f2 == 0.0 {
                        Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                            span: None,
                        }))
                    } else {
                        Ok(LPCValue::Float(f / f2))
                    }
                },
                LPCValue::Int(i) => {
                    if *i == 0 {
                        Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                            span: None,
                        }))
                    } else {
                        Ok(LPCValue::Float(f / *i as f64))
                    }
                }
                _ => Err(self.to_binary_op_error(BinaryOperation::Div, rhs)),
            },
            LPCValue::Int(i) => match rhs {
                LPCValue::Float(f) => {
                    if *f == 0.0 {
                        Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                            span: None,
                        }))
                    } else {
                        Ok(LPCValue::Float(*i as f64 / f))
                    }
                },
                LPCValue::Int(i2) => {
                    if *i2 == 0 {
                        Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                            span: None,
                        }))
                    } else {
                        Ok(LPCValue::Int(i / i2))
                    }
                }
                _ => Err(self.to_binary_op_error(BinaryOperation::Div, rhs)),
            },
            _ => Err(self.to_binary_op_error(BinaryOperation::Div, rhs)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod test_add {
        use super::*;

        #[test]
        fn test_add_int_int() {
            let int1 = LPCValue::Int(123);
            let int2 = LPCValue::Int(456);
            let result = &int1 + &int2;
            if let Ok(LPCValue::Int(x)) = result {
                assert_eq!(x, 579)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_string_string() {
            let string1 = LPCValue::String("foo".to_string());
            let string2 = LPCValue::String("bar".to_string());
            let result = &string1 + &string2;
            if let Ok(LPCValue::String(x)) = result {
                assert_eq!(x, String::from("foobar"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_string_int() {
            let string = LPCValue::String("foo".to_string());
            let int = LPCValue::Int(123);
            let result = &string + &int;
            if let Ok(LPCValue::String(x)) = result {
                assert_eq!(x, String::from("foo123"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_int_string() {
            let string = LPCValue::String("foo".to_string());
            let int = LPCValue::Int(123);
            let result = &int + &string;
            if let Ok(LPCValue::String(x)) = result {
                assert_eq!(x, String::from("123foo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_float_int() {
            let float = LPCValue::Float(666.66);
            let int = LPCValue::Int(123);
            let result = &float + &int;
            if let Ok(LPCValue::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_int_float() {
            let float = LPCValue::Float(666.66);
            let int = LPCValue::Int(123);
            let result = &int + &float;
            if let Ok(LPCValue::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_array_array() {
            let array = LPCValue::from(vec![LPCVar::Int(123)]);
            let array2 = LPCValue::from(vec![LPCVar::Int(4433)]);
            let result = &array + &array2;

            if let Ok(LPCValue::Array(a)) = result {
                assert_eq!(a, vec![LPCVar::Int(123), LPCVar::Int(4433)])
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_mismatched() {
            let int = LPCValue::Int(123);
            let array = LPCValue::Array(vec![]);
            let result = &int + &array;

            if let Ok(_) = result {
                panic!("int + array should have failed, but didn't!")
            }
        }
    }
    
    mod test_sub {
        use super::*;

        #[test]
        fn test_sub_float_int() {
            let float = LPCValue::Float(666.66);
            let int = LPCValue::Int(123);
            let result = &float - &int;
            println!("asdf {:?}", result);
            if let Ok(LPCValue::Float(x)) = result {
                assert_eq!(x, 543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_sub_int_float() {
            let float = LPCValue::Float(666.66);
            let int = LPCValue::Int(123);
            let result = &int - &float;
            if let Ok(LPCValue::Float(x)) = result {
                assert_eq!(x, -543.66)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_mul {
        use super::*;

        #[test]
        fn test_mul_string_int() {
            let string = LPCValue::String("foo".to_string());
            let int = LPCValue::Int(4);
            let result = &string * &int;
            if let Ok(LPCValue::String(x)) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_int_string() {
            let string = LPCValue::String("foo".to_string());
            let int = LPCValue::Int(4);
            let result = &int * &string;
            if let Ok(LPCValue::String(x)) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_float_int() {
            let float = LPCValue::Float(666.66);
            let int = LPCValue::Int(123);
            let result = &float * &int;
            println!("asdf {:?}", result);
            if let Ok(LPCValue::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_int_float() {
            let float = LPCValue::Float(666.66);
            let int = LPCValue::Int(123);
            let result = &int * &float;
            if let Ok(LPCValue::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_div {
        use super::*;

        #[test]
        fn test_div_float_int() {
            let float = LPCValue::Float(666.66);
            let int = LPCValue::Int(123);
            let result = &float / &int;

            if let Ok(LPCValue::Float(x)) = result {
                assert_eq!(x, 5.42)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_div_int_float() {
            let float = LPCValue::Float(666.66);
            let int = LPCValue::Int(123);
            let result = &int / &float;

            if let Ok(LPCValue::Float(x)) = result {
                assert_eq!(x, 0.18450184501845018)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_div_by_zero() {
            let int = LPCValue::Int(123);
            let zero = LPCValue::Int(0);

            assert!((&int / &zero).is_err());
        }
    }
}
