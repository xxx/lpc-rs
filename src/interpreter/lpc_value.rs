use crate::{
    ast::binary_op_node::BinaryOperation,
    errors::runtime_error::{
        binary_operation_error::BinaryOperationError, division_by_zero_error::DivisionByZeroError,
        RuntimeError,
    },
    interpreter::lpc_var::LpcVar,
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LpcValue {
    Float(f64),
    Int(i64),
    String(String),
    Array(Vec<LpcVar>),
}

impl LpcValue {
    /// Get the type name of the value.
    pub fn type_name(&self) -> &str {
        match self {
            LpcValue::Float(_) => "float",
            LpcValue::Int(_) => "int",
            LpcValue::String(_) => "string",
            LpcValue::Array(_) => "array",
        }
    }

    // Just a refactor of a common operation
    fn to_binary_op_error(&self, op: BinaryOperation, right: &LpcValue) -> RuntimeError {
        let e = BinaryOperationError {
            op,
            left_type: self.type_name().to_string(),
            right_type: right.type_name().to_string(),
            span: None,
        };

        RuntimeError::BinaryOperationError(e)
    }
}

impl Display for LpcValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcValue::Float(fl) => write!(f, "{}", fl),
            LpcValue::Int(i) => write!(f, "{}", i),
            LpcValue::String(s) => write!(f, "{}", s),
            LpcValue::Array(a) => write!(f, "({{ {:?} }})", a),
        }
    }
}

impl From<&String> for LpcValue {
    fn from(s: &String) -> Self {
        Self::String(String::from(s))
    }
}

impl From<Vec<LpcVar>> for LpcValue {
    fn from(v: Vec<LpcVar>) -> Self {
        Self::Array(v)
    }
}

impl Add for &LpcValue {
    type Output = Result<LpcValue, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LpcValue::Float(f) => match rhs {
                LpcValue::Float(f2) => Ok(LpcValue::Float(f + f2)),
                LpcValue::Int(i) => Ok(LpcValue::Float(f + *i as f64)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Add, rhs)),
            },
            LpcValue::Int(i) => match rhs {
                LpcValue::Float(f) => Ok(LpcValue::Float(*i as f64 + f)),
                LpcValue::Int(i2) => Ok(LpcValue::Int(i + i2)),
                LpcValue::String(s) => Ok(LpcValue::String(i.to_string() + &s)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Add, rhs)),
            },
            LpcValue::String(s) => match rhs {
                LpcValue::String(s2) => Ok(LpcValue::String(s.clone() + s2)),
                LpcValue::Int(i) => Ok(LpcValue::String(s.clone() + &i.to_string())),
                _ => Err(self.to_binary_op_error(BinaryOperation::Add, rhs)),
            },
            LpcValue::Array(vec) => match rhs {
                LpcValue::Array(vec2) => {
                    let mut new_vec = vec.to_vec();
                    new_vec.extend(&*vec2);
                    Ok(LpcValue::Array(new_vec))
                }
                _ => Err(self.to_binary_op_error(BinaryOperation::Add, rhs)),
            },
        }
    }
}

impl Sub for &LpcValue {
    type Output = Result<LpcValue, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            LpcValue::Int(i) => match rhs {
                LpcValue::Float(f) => Ok(LpcValue::Float(*i as f64 - f)),
                LpcValue::Int(i2) => Ok(LpcValue::Int(i - i2)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Sub, rhs)),
            },
            LpcValue::Float(f) => match rhs {
                LpcValue::Float(f2) => Ok(LpcValue::Float(f - f2)),
                LpcValue::Int(i) => Ok(LpcValue::Float(f - *i as f64)),
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

impl Mul for &LpcValue {
    type Output = Result<LpcValue, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            LpcValue::Float(f) => match rhs {
                LpcValue::Float(f2) => Ok(LpcValue::Float(f * f2)),
                LpcValue::Int(i) => Ok(LpcValue::Float(f * *i as f64)),
                _ => Err(self.to_binary_op_error(BinaryOperation::Mul, rhs)),
            },
            LpcValue::Int(i) => match rhs {
                LpcValue::Float(f) => Ok(LpcValue::Float(*i as f64 * f)),
                LpcValue::Int(i2) => Ok(LpcValue::Int(i * i2)),
                LpcValue::String(s) => Ok(LpcValue::String(repeat_string(s, i))),
                _ => Err(self.to_binary_op_error(BinaryOperation::Mul, rhs)),
            },
            LpcValue::String(s) => {
                match rhs {
                    // repeat the string `s`, `i` times
                    LpcValue::Int(i) => Ok(LpcValue::String(repeat_string(s, i))),
                    _ => Err(self.to_binary_op_error(BinaryOperation::Mul, rhs)),
                }
            }
            _ => Err(self.to_binary_op_error(BinaryOperation::Mul, rhs)),
        }
    }
}

impl Div for &LpcValue {
    type Output = Result<LpcValue, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            LpcValue::Float(f) => match rhs {
                LpcValue::Float(f2) => {
                    if *f2 == 0.0 {
                        Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                            span: None,
                        }))
                    } else {
                        Ok(LpcValue::Float(f / f2))
                    }
                }
                LpcValue::Int(i) => {
                    if *i == 0 {
                        Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                            span: None,
                        }))
                    } else {
                        Ok(LpcValue::Float(f / *i as f64))
                    }
                }
                _ => Err(self.to_binary_op_error(BinaryOperation::Div, rhs)),
            },
            LpcValue::Int(i) => match rhs {
                LpcValue::Float(f) => {
                    if *f == 0.0 {
                        Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                            span: None,
                        }))
                    } else {
                        Ok(LpcValue::Float(*i as f64 / f))
                    }
                }
                LpcValue::Int(i2) => {
                    if *i2 == 0 {
                        Err(RuntimeError::DivisionByZeroError(DivisionByZeroError {
                            span: None,
                        }))
                    } else {
                        Ok(LpcValue::Int(i / i2))
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
            let int1 = LpcValue::Int(123);
            let int2 = LpcValue::Int(456);
            let result = &int1 + &int2;
            if let Ok(LpcValue::Int(x)) = result {
                assert_eq!(x, 579)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_string_string() {
            let string1 = LpcValue::String("foo".to_string());
            let string2 = LpcValue::String("bar".to_string());
            let result = &string1 + &string2;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foobar"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_string_int() {
            let string = LpcValue::String("foo".to_string());
            let int = LpcValue::Int(123);
            let result = &string + &int;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foo123"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_int_string() {
            let string = LpcValue::String("foo".to_string());
            let int = LpcValue::Int(123);
            let result = &int + &string;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("123foo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_float_int() {
            let float = LpcValue::Float(666.66);
            let int = LpcValue::Int(123);
            let result = &float + &int;
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_int_float() {
            let float = LpcValue::Float(666.66);
            let int = LpcValue::Int(123);
            let result = &int + &float;
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_array_array() {
            let array = LpcValue::from(vec![LpcVar::Int(123)]);
            let array2 = LpcValue::from(vec![LpcVar::Int(4433)]);
            let result = &array + &array2;

            if let Ok(LpcValue::Array(a)) = result {
                assert_eq!(a, vec![LpcVar::Int(123), LpcVar::Int(4433)])
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_mismatched() {
            let int = LpcValue::Int(123);
            let array = LpcValue::Array(vec![]);
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
            let float = LpcValue::Float(666.66);
            let int = LpcValue::Int(123);
            let result = &float - &int;
            println!("asdf {:?}", result);
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_sub_int_float() {
            let float = LpcValue::Float(666.66);
            let int = LpcValue::Int(123);
            let result = &int - &float;
            if let Ok(LpcValue::Float(x)) = result {
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
            let string = LpcValue::String("foo".to_string());
            let int = LpcValue::Int(4);
            let result = &string * &int;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_int_string() {
            let string = LpcValue::String("foo".to_string());
            let int = LpcValue::Int(4);
            let result = &int * &string;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_float_int() {
            let float = LpcValue::Float(666.66);
            let int = LpcValue::Int(123);
            let result = &float * &int;
            println!("asdf {:?}", result);
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_int_float() {
            let float = LpcValue::Float(666.66);
            let int = LpcValue::Int(123);
            let result = &int * &float;
            if let Ok(LpcValue::Float(x)) = result {
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
            let float = LpcValue::Float(666.66);
            let int = LpcValue::Int(123);
            let result = &float / &int;

            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 5.42)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_div_int_float() {
            let float = LpcValue::Float(666.66);
            let int = LpcValue::Int(123);
            let result = &int / &float;

            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 0.18450184501845018)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_div_by_zero() {
            let int = LpcValue::Int(123);
            let zero = LpcValue::Int(0);

            assert!((&int / &zero).is_err());
        }
    }
}
