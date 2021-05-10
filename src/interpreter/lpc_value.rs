use crate::{
    ast::binary_op_node::BinaryOperation, errors::LpcError, interpreter::lpc_ref::LpcRef, LpcFloat,
    LpcInt,
};
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::{
    collections::HashMap,
    fmt,
    fmt::Display,
    iter::repeat,
    ops::{Add, Div, Mul, Sub},
};

/// An actual LPC value. These are stored in memory, and as constants.
/// They are only used in the interpreter.
#[derive(Eq, Debug, Clone)]
pub enum LpcValue {
    Float(LpcFloat),
    Int(LpcInt),
    String(String),
    Array(Vec<LpcRef>),
    Mapping(HashMap<LpcRef, LpcRef>),
}

impl LpcValue {
    /// Get the type name of the value.
    pub fn type_name(&self) -> &str {
        match self {
            LpcValue::Float(_) => "float",
            LpcValue::Int(_) => "int",
            LpcValue::String(_) => "string",
            LpcValue::Array(_) => "array",
            LpcValue::Mapping(_) => "mapping",
        }
    }

    /// Just a refactor of a common operation
    fn to_error(&self, op: BinaryOperation, right: &LpcValue) -> LpcError {
        LpcError::new(format!(
            "Runtime Error: Mismatched types: ({}) {} ({})",
            self.type_name(),
            op,
            right.type_name()
        ))
    }
}

impl Display for LpcValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcValue::Float(fl) => write!(f, "{}", fl),
            LpcValue::Int(i) => write!(f, "{}", i),
            LpcValue::String(s) => write!(f, "\"{}\"", s),
            LpcValue::Array(a) => write!(f, "({{ {:?} }})", a),
            LpcValue::Mapping(a) => write!(f, "([ {:?} ])", a),
        }
    }
}

impl From<LpcInt> for LpcValue {
    fn from(i: LpcInt) -> Self {
        Self::Int(i)
    }
}

impl From<f64> for LpcValue {
    fn from(f: f64) -> Self {
        Self::Float(LpcFloat::from(f))
    }
}

impl From<&str> for LpcValue {
    fn from(s: &str) -> Self {
        Self::String(String::from(s))
    }
}

impl From<&String> for LpcValue {
    fn from(s: &String) -> Self {
        Self::String(String::from(s))
    }
}

impl From<Vec<LpcRef>> for LpcValue {
    fn from(v: Vec<LpcRef>) -> Self {
        Self::Array(v)
    }
}

impl From<HashMap<LpcRef, LpcRef>> for LpcValue {
    fn from(m: HashMap<LpcRef, LpcRef>) -> Self {
        Self::Mapping(m)
    }
}

impl PartialEq for LpcValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LpcValue::Float(f), LpcValue::Float(f2)) => f == f2,
            (LpcValue::Int(i), LpcValue::Int(i2)) => i == i2,
            (LpcValue::String(s), LpcValue::String(s2)) => s == s2,
            (LpcValue::Array(v), LpcValue::Array(v2)) => v == v2,
            (LpcValue::Mapping(m), LpcValue::Mapping(m2)) => {
                m.keys().collect::<Vec<_>>() == m2.keys().collect::<Vec<_>>()
                    && m.values().collect::<Vec<_>>() == m2.values().collect::<Vec<_>>()
            }
            _ => false,
        }
    }
}

impl Add for &LpcValue {
    type Output = Result<LpcValue, LpcError>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LpcValue::Float(f) => match rhs {
                LpcValue::Float(f2) => Ok(LpcValue::Float(*f + *f2)),
                LpcValue::Int(i) => Ok(LpcValue::Float(*f + *i as f64)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcValue::Int(i) => match rhs {
                LpcValue::Float(f) => Ok(LpcValue::Float(LpcFloat::from(*i as f64) + *f)),
                LpcValue::Int(i2) => Ok(LpcValue::Int(i + i2)),
                LpcValue::String(s) => Ok(LpcValue::String(i.to_string() + &s)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcValue::String(s) => match rhs {
                LpcValue::String(s2) => Ok(LpcValue::String(s.clone() + s2)),
                LpcValue::Int(i) => Ok(LpcValue::String(s.clone() + &i.to_string())),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcValue::Array(vec) => match rhs {
                LpcValue::Array(vec2) => {
                    let mut new_vec = vec.to_vec();
                    new_vec.extend(vec2.clone().into_iter());
                    Ok(LpcValue::Array(new_vec))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcValue::Mapping(map) => match rhs {
                LpcValue::Mapping(map2) => {
                    let mut new_map = map.clone();
                    new_map.extend(map2.clone().into_iter());
                    Ok(LpcValue::Mapping(new_map))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
        }
    }
}

impl Sub for &LpcValue {
    type Output = Result<LpcValue, LpcError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            LpcValue::Int(i) => match rhs {
                LpcValue::Float(f) => Ok(LpcValue::Float(LpcFloat::from(*i as f64) - *f)),
                LpcValue::Int(i2) => Ok(LpcValue::Int(i - i2)),
                _ => Err(self.to_error(BinaryOperation::Sub, rhs)),
            },
            LpcValue::Float(f) => match rhs {
                LpcValue::Float(f2) => Ok(LpcValue::Float(*f - *f2)),
                LpcValue::Int(i) => Ok(LpcValue::Float(*f - *i as f64)),
                _ => Err(self.to_error(BinaryOperation::Sub, rhs)),
            },
            _ => Err(self.to_error(BinaryOperation::Sub, rhs)),
        }
    }
}

/// Repeat `s`, `i` times, and return a new String of it.
fn repeat_string(s: &str, i: &LpcInt) -> String {
    if *i >= 0 {
        repeat(s).take(*i as usize).collect::<String>()
    } else {
        String::from("")
    }
}

impl Mul for &LpcValue {
    type Output = Result<LpcValue, LpcError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            LpcValue::Float(f) => match rhs {
                LpcValue::Float(f2) => Ok(LpcValue::Float(*f * *f2)),
                LpcValue::Int(i) => Ok(LpcValue::Float(*f * *i as f64)),
                _ => Err(self.to_error(BinaryOperation::Mul, rhs)),
            },
            LpcValue::Int(i) => match rhs {
                LpcValue::Float(f) => Ok(LpcValue::Float(LpcFloat::from(*i as f64) * *f)),
                LpcValue::Int(i2) => Ok(LpcValue::Int(i * i2)),
                LpcValue::String(s) => Ok(LpcValue::String(repeat_string(s, i))),
                _ => Err(self.to_error(BinaryOperation::Mul, rhs)),
            },
            LpcValue::String(s) => {
                match rhs {
                    // repeat the string `s`, `i` times
                    LpcValue::Int(i) => Ok(LpcValue::String(repeat_string(s, i))),
                    _ => Err(self.to_error(BinaryOperation::Mul, rhs)),
                }
            }
            _ => Err(self.to_error(BinaryOperation::Mul, rhs)),
        }
    }
}

impl Div for &LpcValue {
    type Output = Result<LpcValue, LpcError>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            LpcValue::Float(f) => match rhs {
                LpcValue::Float(f2) => {
                    if *f2 == 0.0 {
                        Err(LpcError::new("Runtime Error: Division by zero"))
                    } else {
                        Ok(LpcValue::Float(*f / *f2))
                    }
                }
                LpcValue::Int(i) => {
                    if *i == 0 {
                        Err(LpcError::new("Runtime Error: Division by zero"))
                    } else {
                        Ok(LpcValue::Float(*f / *i as f64))
                    }
                }
                _ => Err(self.to_error(BinaryOperation::Div, rhs)),
            },
            LpcValue::Int(i) => match rhs {
                LpcValue::Float(f) => {
                    if *f == 0.0 {
                        Err(LpcError::new("Runtime Error: Division by zero"))
                    } else {
                        Ok(LpcValue::Float(LpcFloat::from(*i as f64) / *f))
                    }
                }
                LpcValue::Int(i2) => {
                    if *i2 == 0 {
                        Err(LpcError::new("Runtime Error: Division by zero"))
                    } else {
                        Ok(LpcValue::Int(i / i2))
                    }
                }
                _ => Err(self.to_error(BinaryOperation::Div, rhs)),
            },
            _ => Err(self.to_error(BinaryOperation::Div, rhs)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod test_add {
        use super::*;
        use refpool::{Pool, PoolRef};
        use std::cell::RefCell;

        fn to_ref(pool: &Pool<RefCell<LpcValue>>, v: LpcValue) -> LpcRef {
            match v {
                LpcValue::Float(f) => LpcRef::Float(f),
                LpcValue::Int(i) => LpcRef::Int(i),
                LpcValue::String(s) => {
                    LpcRef::String(PoolRef::new(pool, RefCell::new(LpcValue::String(s))))
                }
                LpcValue::Array(a) => {
                    LpcRef::Array(PoolRef::new(pool, RefCell::new(LpcValue::Array(a))))
                }
                LpcValue::Mapping(m) => {
                    LpcRef::Mapping(PoolRef::new(pool, RefCell::new(LpcValue::Mapping(m))))
                }
            }
        }

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
            let float = LpcValue::from(666.66);
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
            let float = LpcValue::from(666.66);
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
            let array = LpcValue::from(vec![LpcRef::Int(123)]);
            let array2 = LpcValue::from(vec![LpcRef::Int(4433)]);
            let result = &array + &array2;

            if let Ok(LpcValue::Array(a)) = result {
                assert_eq!(a, vec![LpcRef::Int(123), LpcRef::Int(4433)])
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_mapping_mapping() {
            let pool = Pool::new(20);
            let key1 = to_ref(&pool, LpcValue::from("key1"));
            let value1 = to_ref(&pool, LpcValue::from("value1"));
            let key2 = to_ref(&pool, LpcValue::from("key2"));
            let value2 = to_ref(&pool, LpcValue::from(666));

            let mut hash1 = HashMap::new();
            hash1.insert(key1.clone(), value1.clone());

            let mut hash2 = HashMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = LpcValue::from(hash1);
            let map2 = LpcValue::from(hash2);

            let result = &map + &map2;

            let mut expected = HashMap::new();
            expected.insert(key1, value1);
            expected.insert(key2, value2);

            if let Ok(LpcValue::Mapping(m)) = result {
                assert_eq!(m, expected)
            } else {
                panic!("no match. received: {:?}", result)
            }
        }

        #[test]
        fn test_add_mapping_mapping_duplicate_keys() {
            let pool = Pool::new(20);
            let key1 = to_ref(&pool, LpcValue::from("key"));
            let value1 = to_ref(&pool, LpcValue::from("value1"));
            let key2 = to_ref(&pool, LpcValue::from("key"));
            let value2 = to_ref(&pool, LpcValue::from(666));

            let mut hash1 = HashMap::new();
            hash1.insert(key1, value1);

            let mut hash2 = HashMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = LpcValue::from(hash1);
            let map2 = LpcValue::from(hash2);

            let result = &map + &map2;

            let mut expected = HashMap::new();
            expected.insert(key2, value2);

            if let Ok(LpcValue::Mapping(m)) = result {
                assert_eq!(m, expected)
            } else {
                panic!("no match. received: {:?}", result)
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
            let float = LpcValue::from(666.66);
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
            let float = LpcValue::from(666.66);
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
            let float = LpcValue::from(666.66);
            let int = LpcValue::Int(123);
            let result = &float * &int;
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_int_float() {
            let float = LpcValue::from(666.66);
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
            let float = LpcValue::from(666.66);
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
            let float = LpcValue::from(666.66);
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
