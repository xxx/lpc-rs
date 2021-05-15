use crate::{
    ast::binary_op_node::BinaryOperation, errors::LpcError, interpreter::lpc_value::LpcValue,
    LpcFloat, LpcInt,
    try_extract_value
};
use refpool::PoolRef;
use std::{
    cell::RefCell,
    fmt,
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    iter::repeat,
    ops::{Add, Div, Mul, Sub},
    ptr,
};

#[macro_export]
/// Convert an LpcValue into an LpcRef, wrapping heap values as necessary
///
/// # Arguments
/// `r` - The expression to be wrapped.
/// `m` - The memory pool to declare the [`PoolRef`]s from.
macro_rules! value_to_ref {
    ($r:expr, $m:expr) => {
        match $r {
            LpcValue::Float(x) => LpcRef::Float(x),
            LpcValue::Int(x) => LpcRef::Int(x),
            LpcValue::String(x) => {
                LpcRef::String(PoolRef::new(&$m, RefCell::new(LpcValue::String(x))))
            }
            LpcValue::Array(x) => {
                LpcRef::Array(PoolRef::new(&$m, RefCell::new(LpcValue::Array(x))))
            }
            LpcValue::Mapping(x) => {
                LpcRef::Mapping(PoolRef::new(&$m, RefCell::new(LpcValue::Mapping(x))))
            }
        }
    };
}

/// A more dangerous version of [`try_extract_value`], that panics instead.
/// Only intended for use in this file (and tests may use it at-will)
#[macro_export]
macro_rules! extract_value {
    ( $x:expr, $y:path ) => {
        match &$x {
            $y(s) => s,
            x => panic!("Invalid LpcValue - received `{}`. This indicates a serious bug in the interpreter.", x)
        }
    };
}

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

impl From<f64> for LpcRef {
    fn from(f: f64) -> Self {
        Self::Float(LpcFloat::from(f))
    }
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
            _ => false,
        }
    }
}

impl Display for LpcRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcRef::Float(x) => write!(f, "{}", x),
            LpcRef::Int(x) => write!(f, "{}", x),
            LpcRef::String(x) | LpcRef::Array(x) | LpcRef::Mapping(x) => {
                write!(f, "{}", x.borrow())
            }
        }
    }
}

impl Add for &LpcRef {
    type Output = Result<LpcValue, LpcError>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LpcRef::Float(f) => match rhs {
                LpcRef::Float(f2) => Ok(LpcValue::Float(*f + *f2)),
                LpcRef::Int(i) => Ok(LpcValue::Float(*f + *i as f64)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Int(i) => match rhs {
                LpcRef::Float(f) => Ok(LpcValue::Float(LpcFloat::from(*i as f64) + *f)),
                LpcRef::Int(i2) => Ok(LpcValue::Int(i + i2)),
                LpcRef::String(s) => Ok(LpcValue::String(
                    i.to_string() + try_extract_value!(*s.borrow(), LpcValue::String),
                )),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::String(s) => match rhs {
                LpcRef::String(s2) => Ok(LpcValue::String(
                    try_extract_value!(*s.borrow(), LpcValue::String).clone()
                        + try_extract_value!(*s2.borrow(), LpcValue::String),
                )),
                LpcRef::Int(i) => Ok(LpcValue::String(
                    try_extract_value!(*s.borrow(), LpcValue::String).clone() + &i.to_string(),
                )),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Array(vec) => match rhs {
                LpcRef::Array(vec2) => {
                    let mut new_vec = try_extract_value!(*vec.borrow(), LpcValue::Array).clone();
                    let added_vec = try_extract_value!(*vec2.borrow(), LpcValue::Array).clone();
                    new_vec.extend(added_vec.into_iter());
                    Ok(LpcValue::Array(new_vec))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Mapping(map) => match rhs {
                LpcRef::Mapping(map2) => {
                    let mut new_map = try_extract_value!(*map.borrow(), LpcValue::Mapping).clone();
                    let added_map = try_extract_value!(*map2.borrow(), LpcValue::Mapping).clone();
                    new_map.extend(added_map.into_iter());
                    Ok(LpcValue::Mapping(new_map))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
        }
    }
}

impl Sub for &LpcRef {
    type Output = Result<LpcValue, LpcError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcValue::Int(*x - *y)),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(LpcValue::Float(*x - *y)),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(LpcValue::Float(*x - *y as f64)),
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                Ok(LpcValue::Float(LpcFloat::from(*x as f64) - *y))
            }
            _ => Err(self.to_error(BinaryOperation::Sub, &rhs)),
        }
    }
}

/// Repeat `s`, `i` times, and return a new String of it.
fn repeat_string(s: &str, i: LpcInt) -> String {
    if i >= 0 {
        repeat(s).take(i as usize).collect()
    } else {
        String::from("")
    }
}

impl Mul for &LpcRef {
    type Output = Result<LpcValue, LpcError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcValue::Int(*x * *y)),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(LpcValue::Float(*x * *y)),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(LpcValue::Float(*x * *y as f64)),
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                Ok(LpcValue::Float(LpcFloat::from(*x as f64) * *y))
            }
            (LpcRef::String(x), LpcRef::Int(y)) => {
                let b = x.borrow();
                let string = try_extract_value!(*b, LpcValue::String);
                Ok(LpcValue::String(repeat_string(string, *y)))
            }
            (LpcRef::Int(x), LpcRef::String(y)) => {
                let b = y.borrow();
                let string = try_extract_value!(*b, LpcValue::String);
                Ok(LpcValue::String(repeat_string(string, *x)))
            }
            _ => Err(self.to_error(BinaryOperation::Mul, &rhs)),
        }
    }
}

impl Div for &LpcRef {
    type Output = Result<LpcValue, LpcError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                if y == &0 {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Int(*x / *y))
                }
            }
            (LpcRef::Float(x), LpcRef::Float(y)) => {
                if (*y - LpcFloat::from(0.0)).into_inner().abs() < f64::EPSILON {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Float(*x / *y))
                }
            }
            (LpcRef::Float(x), LpcRef::Int(y)) => {
                if y == &0 {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Float(*x / *y as f64))
                }
            }
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                if (*y - LpcFloat::from(0.0)).into_inner().abs() < f64::EPSILON {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Float(LpcFloat::from(*x as f64) / *y))
                }
            }
            _ => Err(self.to_error(BinaryOperation::Div, &rhs)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use refpool::{Pool, PoolRef};
    use std::{cell::RefCell, collections::HashMap};

    mod test_add {
        use super::*;

        #[test]
        fn test_add_int_int() {
            let int1 = LpcRef::Int(123);
            let int2 = LpcRef::Int(456);
            let result = &int1 + &int2;
            if let Ok(LpcValue::Int(x)) = result {
                assert_eq!(x, 579)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_string_string() {
            let pool = Pool::new(20);
            let string1 = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let string2 = value_to_ref!(LpcValue::String("bar".to_string()), pool);
            let result = &string1 + &string2;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foobar"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_string_int() {
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let int = LpcRef::Int(123);
            let result = &string + &int;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foo123"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_int_string() {
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let int = LpcRef::Int(123);
            let result = &int + &string;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("123foo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = &float + &int;
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = &int + &float;
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_add_array_array() {
            let pool = Pool::new(20);
            let array = LpcValue::from(vec![LpcRef::Int(123)]);
            let array2 = LpcValue::from(vec![LpcRef::Int(4433)]);
            let result = &value_to_ref!(array.clone(), pool) + &value_to_ref!(array2, pool);

            match &result {
                Ok(v) => {
                    assert_ne!(
                        extract_value!(v, LpcValue::Array),
                        extract_value!(array, LpcValue::Array)
                    ); // ensure the addition makes a fully new copy

                    if let LpcValue::Array(a) = v {
                        assert_eq!(a, &vec![LpcRef::Int(123), LpcRef::Int(4433)]);
                    } else {
                        panic!("no match")
                    }
                }
                _ => panic!("no match"),
            }
        }

        #[test]
        fn test_add_mapping_mapping() {
            let pool = Pool::new(20);
            let key1 = value_to_ref!(LpcValue::from("key1"), pool);
            let value1 = value_to_ref!(LpcValue::from("value1"), pool);
            let key2 = value_to_ref!(LpcValue::from("key2"), pool);
            let value2 = value_to_ref!(LpcValue::from(666), pool);

            let mut hash1 = HashMap::new();
            hash1.insert(key1.clone(), value1.clone());

            let mut hash2 = HashMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = value_to_ref!(LpcValue::from(hash1), pool);
            let map2 = value_to_ref!(LpcValue::from(hash2), pool);

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
            let key1 = value_to_ref!(LpcValue::from("key"), pool);
            let value1 = value_to_ref!(LpcValue::from("value1"), pool);
            let key2 = value_to_ref!(LpcValue::from("key"), pool);
            let value2 = value_to_ref!(LpcValue::from(666), pool);

            let mut hash1 = HashMap::new();
            hash1.insert(key1, value1);

            let mut hash2 = HashMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = value_to_ref!(LpcValue::from(hash1), pool);
            let map2 = value_to_ref!(LpcValue::from(hash2), pool);

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
            let pool = Pool::new(5);
            let int = LpcRef::Int(123);
            let array = value_to_ref!(LpcValue::Array(vec![]), pool);
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
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = &float - &int;
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_sub_int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
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
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let int = LpcRef::Int(4);
            let result = &string * &int;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_int_string() {
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let int = LpcRef::Int(4);
            let result = &int * &string;
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = &float * &int;
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_mul_int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
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
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = &float / &int;

            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 5.42)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_div_int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = &int / &float;

            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 0.18450184501845018)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn test_div_by_zero() {
            let int = LpcRef::Int(123);
            let zero = LpcRef::Int(0);

            assert!((&int / &zero).is_err());
        }
    }
}
