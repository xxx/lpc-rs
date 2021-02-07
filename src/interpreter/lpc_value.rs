use std::ops::{Add, Mul, Sub, Div};
use std::iter::repeat;
use crate::interpreter::lpc_var::LPCVar;

/// An actual LPC value. These are stored in memory, and as constants.
/// They are only used in the interpreter.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LPCValue {
    Int(i64),
    String(String),
    Array(Vec<LPCVar>)
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
    type Output = Option<LPCValue>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
                match rhs {
                    LPCValue::Int(i2) => Some(LPCValue::Int(i + i2)),
                    LPCValue::String(s) => Some(LPCValue::String(i.to_string() + &s)),
                    _ => None
                }
            }
            LPCValue::String(s) => {
                match rhs {
                    LPCValue::String(s2) => Some(LPCValue::String(s.clone() + s2)),
                    LPCValue::Int(i) => Some(LPCValue::String(s.clone() + &i.to_string())),
                    _ => None
                }
            }
            LPCValue::Array(vec) => {
                match rhs {
                    LPCValue::Array(vec2) => {
                        let mut new_vec = vec.to_vec();
                        new_vec.extend(&*vec2);
                        Some(LPCValue::Array(new_vec))
                    },
                    _ => None
                }
            }
        }
    }
}

impl Sub for &LPCValue {
    type Output = Option<LPCValue>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
                match rhs {
                    LPCValue::Int(i2) => Some(LPCValue::Int(i - i2)),
                    _ => None,
                }
            }
            _ => None
        }
    }
}

/// Repeat `s`, `i` times, and return a new String of it.
fn repeat_string(s: &str, i: &i64) -> String {
    if *i >= 0 {
        repeat(s.clone()).take(*i as usize).collect::<String>()
    } else {
        String::from("")
    }
}

impl Mul for &LPCValue {
    type Output = Option<LPCValue>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
                match rhs {
                    LPCValue::Int(i2) => Some(LPCValue::Int(i * i2)),
                    LPCValue::String(s) => {
                        Some(LPCValue::String(repeat_string(s, i)))
                    },
                    _ => None,
                }
            }
            LPCValue::String(s) => {
                match rhs {
                    // repeat the string `s`, `i` times
                    LPCValue::Int(i) => {
                        Some(LPCValue::String(repeat_string(s, i)))
                    }
                    _ => None,
                }
            },
            _ => None
        }
    }
}

impl Div for &LPCValue {
    type Output = Option<LPCValue>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
                match rhs {
                    LPCValue::Int(i2) => Some(LPCValue::Int(i / i2)),
                    _ => None,
                }
            }
            _ => None
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
            if let LPCValue::Int(x) = result.unwrap() {
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
            if let LPCValue::String(x) = result.unwrap() {
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
            if let LPCValue::String(x) = result.unwrap() {
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
            if let LPCValue::String(x) = result.unwrap() {
                assert_eq!(x, String::from("123foo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_array_array() {
            let array = LPCValue::from(vec![LPCVar::Int(123)]);
            let array2 = LPCValue::from(vec![LPCVar::Int(4433)]);
            let result = &array + &array2;

            if let LPCValue::Array(a) = result.unwrap() {
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
            assert_eq!(result, None);
        }
    }
    
    mod test_mul {
        use super::*;
        
        #[test]
        fn test_mul_string_int() {
            let string = LPCValue::String("foo".to_string());
            let int = LPCValue::Int(4);
            let result = &string * &int;
            if let LPCValue::String(x) = result.unwrap() {
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
            if let LPCValue::String(x) = result.unwrap() {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }
    }
}
