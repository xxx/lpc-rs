use std::ops::{Add, Mul, Sub, Div};
use std::iter::repeat;
use crate::interpreter::lpc_var::LPCVar;

/// An actual LPC value. These are stored in memory, and as constants.
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
    type Output = LPCValue;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
                match rhs {
                    LPCValue::Int(i2) => LPCValue::Int(i + i2),
                    LPCValue::String(s) => LPCValue::String(i.to_string() + &s),
                    _ => unimplemented!()
                }
            }
            LPCValue::String(s) => {
                match rhs {
                    LPCValue::String(s2) => LPCValue::String(s.clone() + s2),
                    LPCValue::Int(i) => LPCValue::String(s.clone() + &i.to_string()),
                    _ => unimplemented!()
                }
            }
            LPCValue::Array(vec) => {
                match rhs {
                    LPCValue::Array(vec2) => {
                        let mut new_vec = vec.to_vec();
                        new_vec.extend(&*vec2);
                        LPCValue::Array(new_vec)
                    },
                    _ => unimplemented!()
                }
            }
        }
    }
}

impl Sub for &LPCValue {
    type Output = LPCValue;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
                match rhs {
                    LPCValue::Int(i2) => LPCValue::Int(i - i2),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!()
        }
    }
}

impl Mul for &LPCValue {
    type Output = LPCValue;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
                match rhs {
                    LPCValue::Int(i2) => LPCValue::Int(i * i2),
                    _ => unimplemented!(),
                }
            }
            LPCValue::String(s) => {
                match rhs {
                    // repeat the string `s`, `i` times
                    LPCValue::Int(i) => {
                        let value = if *i >= 0 {
                            repeat(s.clone()).take(*i as usize).collect::<String>()
                        } else {
                            String::from("")
                        };

                        LPCValue::String(value)
                    }
                    _ => unimplemented!(),
                }
            },
            _ => unimplemented!()
        }
    }
}

impl Div for &LPCValue {
    type Output = LPCValue;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
                match rhs {
                    LPCValue::Int(i2) => LPCValue::Int(i / i2),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!()
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
            if let LPCValue::Int(x) = result {
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
            if let LPCValue::String(x) = result {
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
            if let LPCValue::String(x) = result {
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
            if let LPCValue::String(x) = result {
                assert_eq!(x, String::from("123foo"))
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
            if let LPCValue::String(x) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }
    }
}
