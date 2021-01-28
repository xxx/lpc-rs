use std::ops::{Add, Mul};
use std::iter::repeat;

/// An actual LPC value. These are stored as constants in the ConstantPool.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LPCValue {
    Int(i64),
    String(String)
}

impl From<&String> for LPCValue {
    fn from(s: &String) -> Self {
        Self::String(String::from(s))
    }
}

impl Add for &LPCValue {
    type Output = LPCValue;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LPCValue::Int(i) => {
              match rhs {
                  LPCValue::Int(i2) => LPCValue::Int(i + i2),
                  _ => unimplemented!()
              }
            },
            LPCValue::String(s) => {
                match rhs {
                    LPCValue::String(s2) => LPCValue::String(s.clone() + s2),
                    LPCValue::Int(i) => LPCValue::String(s.clone() + &i.to_string())
                }
            },
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
                    _ => unimplemented!()
                }
            },
            LPCValue::String(s) => {
                match rhs {
                    // repeat the string `s`, `i` times
                    LPCValue::Int(i) => {
                        let value = if *i >= 0 {
                            repeat(s.clone())
                                .take(*i as usize).collect::<String>()
                        } else {
                            String::from("")
                        };

                        LPCValue::String(value)
                    },
                    _ => unimplemented!()
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_int_plus_int() {
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
    fn test_add_string_plus_string() {
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
    fn test_add_string_plus_int() {
        let string = LPCValue::String("foo".to_string());
        let int = LPCValue::Int(123);
        let result = &string + &int;
        if let LPCValue::String(x) = result {
            assert_eq!(x, String::from("foo123"))
        } else {
            panic!("no match")
        }
    }
}