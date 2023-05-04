use std::{
    cmp::Ordering,
    fmt,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    ptr,
    sync::Weak,
};

use bit_set::BitSet;
use lpc_rs_core::{lpc_type::LpcType, BaseFloat, LpcFloatInner, LpcIntInner};
use lpc_rs_errors::{lpc_error, LpcError, Result};
use lpc_rs_utils::{string, string::concatenate_strings};
use parking_lot::RwLock;
use shared_arena::ArenaArc;
use tracing::{instrument, trace};

use crate::{
    compiler::ast::{binary_op_node::BinaryOperation, unary_op_node::UnaryOperation},
    interpreter::{
        function_type::function_ptr::FunctionPtr, gc::mark::Mark, heap::Heap,
        into_lpc_ref::IntoLpcRef, lpc_array::LpcArray, lpc_float::LpcFloat, lpc_int::LpcInt,
        lpc_mapping::LpcMapping, lpc_string::LpcString, process::Process,
    },
};

pub const NULL: LpcRef = LpcRef::Int(LpcInt(0));

/// Represent a variable stored in a `Register`. Value types store the actual
/// value. Reference types store a reference to the actual value.
/// This type is intended to be cheap to clone.
#[derive(Clone)]
pub enum LpcRef {
    Float(LpcFloat),
    Int(LpcInt),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value
    String(ArenaArc<RwLock<LpcString>>),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value
    Array(ArenaArc<RwLock<LpcArray>>),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value
    Mapping(ArenaArc<RwLock<LpcMapping>>),

    /// Reference type, pointing to an LPC `object`
    Object(ArenaArc<Weak<Process>>),

    /// Reference type, a function pointer or closure
    Function(ArenaArc<FunctionPtr>),
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
            LpcRef::Object(_) => "object",
            LpcRef::Function(_) => "function",
        }
    }

    /// Is this a null ref?
    pub fn is_null(&self) -> bool {
        matches!(self, LpcRef::Int(LpcInt(0)))
    }

    fn to_error(&self, op: BinaryOperation, right: &LpcRef) -> Box<LpcError> {
        lpc_error!(
            "runtime error: mismatched types: {} ({}) {} {} ({})",
            self,
            self.type_name(),
            op,
            right,
            right.type_name()
        )
    }

    fn to_unary_op_error(&self, op: UnaryOperation) -> Box<LpcError> {
        lpc_error!(
            "runtime error: mismatched types: {} {} ({})",
            op,
            self,
            self.type_name()
        )
    }

    pub fn inc(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(ref mut x) => {
                *x = x.wrapping_add(1).into();

                Ok(())
            }
            _ => Err(lpc_error!("runtime error: invalid increment")),
        }
    }

    pub fn dec(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(ref mut x) => {
                *x = x.wrapping_sub(1).into();
                Ok(())
            }
            _ => Err(lpc_error!("runtime error: invalid decrement")),
        }
    }

    pub fn as_lpc_type(&self) -> LpcType {
        match self {
            LpcRef::Float(_) => LpcType::Float(false),
            LpcRef::Int(_) => LpcType::Int(false),
            LpcRef::String(_) => LpcType::String(false),
            LpcRef::Array(_) => LpcType::Mixed(true), // this could be better
            LpcRef::Mapping(_) => LpcType::Mapping(false),
            LpcRef::Object(_) => LpcType::Object(false),
            LpcRef::Function(_) => LpcType::Function(false),
        }
    }

    /// Convenience to perform a binary operation on a pair of [`LpcRef`]s
    /// wrapped in RwLocks.
    pub fn binary_op<F, T>(left: &RwLock<Self>, right: &RwLock<Self>, op: F) -> T
    where
        F: Fn(&LpcRef, &LpcRef) -> T,
    {
        let left = &*left.read();
        let right = &*right.read();

        op(left, right)
    }

    pub fn add(&self, rhs: &Self, memory: &Heap) -> Result<Self> {
        match self {
            LpcRef::Float(f) => match rhs {
                LpcRef::Float(f2) => Ok(Self::from(*f + *f2)),
                LpcRef::Int(i) => Ok(Self::from(*f + *i)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Int(i) => match rhs {
                LpcRef::Float(f) => Ok(Self::from(*i + *f)),
                LpcRef::Int(i2) => Ok(Self::from(*i + *i2)),
                LpcRef::String(s) => Ok(LpcString::from(concatenate_strings(
                    i.to_string(),
                    s.read().to_str(),
                )?)
                .into_lpc_ref(memory)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::String(s) => match rhs {
                LpcRef::String(s2) => Ok(LpcString::from(concatenate_strings(
                    s.read().to_string(),
                    s2.read().to_str(),
                )?)
                .into_lpc_ref(memory)),
                LpcRef::Int(i) => Ok(LpcString::from(concatenate_strings(
                    s.read().to_string(),
                    i.to_string(),
                )?)
                .into_lpc_ref(memory)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Array(vec) => match rhs {
                LpcRef::Array(vec2) => {
                    let new_vec = vec.read();
                    let added_vec = vec2.read();
                    Ok((new_vec.clone() + added_vec.clone()).into_lpc_ref(memory))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Mapping(map) => match rhs {
                LpcRef::Mapping(map2) => {
                    let mut new_map = map.read().clone();
                    let added_map = map2.read().clone();
                    new_map.extend(added_map.into_iter());
                    Ok(new_map.into_lpc_ref(memory))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Object(_) | LpcRef::Function(_) => {
                Err(self.to_error(BinaryOperation::Add, rhs))
            }
        }
    }

    pub fn sub(&self, rhs: &Self, memory: &Heap) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(Self::Int(x.wrapping_sub(y.0).into())),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(Self::Float((x.0 - y.0).into())),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(Self::Float((x.0 - y.0 as BaseFloat).into())),
            (LpcRef::Int(x), LpcRef::Float(y)) => Ok(Self::Float(LpcFloat::from(
                LpcFloatInner::from(x.0 as BaseFloat) - y.0,
            ))),
            (LpcRef::Array(vec), LpcRef::Array(vec2)) => {
                let new_vec = vec.read().clone();
                let removed_vec = vec2.read().clone();

                let result = new_vec
                    .into_iter()
                    .filter(|x| !removed_vec.contains(x))
                    .collect::<LpcArray>();
                Ok(result.into_lpc_ref(memory))
            }
            _ => Err(self.to_error(BinaryOperation::Sub, rhs)),
        }
    }

    pub fn mul(&self, rhs: &Self, memory: &Heap) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(Self::Int(x.0.wrapping_mul(y.0).into())),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(Self::Float((x.0 * y.0).into())),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(Self::Float((x.0 * y.0 as BaseFloat).into())),
            (LpcRef::Int(x), LpcRef::Float(y)) => Ok(Self::Float(
                (LpcFloatInner::from(x.0 as BaseFloat) * y.0).into(),
            )),
            (LpcRef::String(x), LpcRef::Int(y)) => {
                let string = x.read();
                Ok(
                    LpcString::from(string::repeat_string(string.to_str(), y.0)?)
                        .into_lpc_ref(memory),
                )
            }
            (LpcRef::Int(x), LpcRef::String(y)) => {
                let string = y.read();
                Ok(
                    LpcString::from(string::repeat_string(string.to_str(), x.0)?)
                        .into_lpc_ref(memory),
                )
            }
            _ => Err(self.to_error(BinaryOperation::Mul, rhs)),
        }
    }

    pub fn div(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                if y.0 == 0 {
                    Err(lpc_error!("Runtime Error: Division by zero"))
                } else {
                    Ok(Self::Int(LpcInt(x.0.wrapping_div(y.0))))
                }
            }
            (LpcRef::Float(x), LpcRef::Float(y)) => {
                if (y.0 - LpcFloatInner::from(0.0)).into_inner().abs() < BaseFloat::EPSILON {
                    Err(lpc_error!("Runtime Error: Division by zero"))
                } else {
                    Ok(Self::Float(LpcFloat(x.0 / y.0)))
                }
            }
            (LpcRef::Float(x), LpcRef::Int(y)) => {
                if y.0 == 0 {
                    Err(lpc_error!("Runtime Error: Division by zero"))
                } else {
                    Ok(Self::Float(LpcFloat(x.0 / y.0 as BaseFloat)))
                }
            }
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                if (y.0 - LpcFloatInner::from(0.0)).into_inner().abs() < BaseFloat::EPSILON {
                    Err(lpc_error!("Runtime Error: Division by zero"))
                } else {
                    Ok(Self::Float(LpcFloat(
                        LpcFloatInner::from(x.0 as BaseFloat) / y.0,
                    )))
                }
            }
            _ => Err(self.to_error(BinaryOperation::Div, rhs)),
        }
    }

    pub fn rem(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                if y.0 == 0 {
                    Err(lpc_error!("Runtime Error: Remainder division by zero"))
                } else {
                    Ok(Self::Int(LpcInt(x.0.wrapping_rem(y.0))))
                }
            }
            (LpcRef::Float(x), LpcRef::Float(y)) => {
                if (y.0 - LpcFloatInner::from(0.0)).into_inner().abs() < BaseFloat::EPSILON {
                    Err(lpc_error!("Runtime Error: Division by zero"))
                } else {
                    Ok(Self::Float(LpcFloat(x.0 % y.0)))
                }
            }
            (LpcRef::Float(x), LpcRef::Int(y)) => {
                if y.0 == 0 {
                    Err(lpc_error!("Runtime Error: Division by zero"))
                } else {
                    Ok(Self::Float(LpcFloat(x.0 % y.0 as BaseFloat)))
                }
            }
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                if (y.0 - LpcFloatInner::from(0.0)).into_inner().abs() < BaseFloat::EPSILON {
                    Err(lpc_error!("Runtime Error: Remainder division by zero"))
                } else {
                    Ok(Self::Float(LpcFloat(
                        LpcFloatInner::from(x.0 as BaseFloat) % y.0,
                    )))
                }
            }
            _ => Err(self.to_error(BinaryOperation::Mod, rhs)),
        }
    }

    pub fn bitand(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(Self::Int(LpcInt(x.0 & y.0))),
            _ => Err(self.to_error(BinaryOperation::And, rhs)),
        }
    }

    pub fn bitor(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(Self::Int(LpcInt(x.0 | y.0))),
            _ => Err(self.to_error(BinaryOperation::Or, rhs)),
        }
    }

    pub fn bitxor(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(Self::Int(LpcInt(x.0 ^ y.0))),
            _ => Err(self.to_error(BinaryOperation::Xor, rhs)),
        }
    }

    pub fn shl(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                let modulo: LpcIntInner = y.0 % (LpcIntInner::BITS as LpcIntInner);

                let shift_by: u32 = if modulo < 0 {
                    LpcIntInner::BITS - (modulo.unsigned_abs() as u32)
                } else {
                    modulo as u32
                };

                Ok(Self::Int(LpcInt(x.0.checked_shl(shift_by).unwrap_or(0))))
            }
            _ => Err(self.to_error(BinaryOperation::Shl, rhs)),
        }
    }

    pub fn shr(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                let modulo: LpcIntInner = y.0 % (LpcIntInner::BITS as LpcIntInner);

                let shift_by: u32 = if modulo < 0 {
                    LpcIntInner::BITS - (modulo.unsigned_abs() as u32)
                } else {
                    modulo as u32
                };

                Ok(Self::Int(LpcInt(x.0.checked_shr(shift_by).unwrap_or(0))))
            }
            _ => Err(self.to_error(BinaryOperation::Shr, rhs)),
        }
    }

    /// Impl _bitwise_ Not for ints, (i.e. the unary `~` operator)
    pub fn bitnot(&self) -> Result<Self> {
        match &self {
            LpcRef::Int(x) => Ok(Self::Int(LpcInt(!x.0))),
            _ => Err(self.to_unary_op_error(UnaryOperation::BitwiseNot)),
        }
    }
}

impl Mark for LpcRef {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        trace!("marking lpc ref of {type}", type = self.as_lpc_type());

        match self {
            LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::String(_) | LpcRef::Object(_) => Ok(()),
            LpcRef::Array(arr) => {
                let arr = arr.read();
                arr.mark(marked, processed)
            }
            LpcRef::Mapping(map) => {
                let map = map.read();
                map.mark(marked, processed)
            }
            LpcRef::Function(fun) => fun.mark(marked, processed),
        }
    }
}

impl From<BaseFloat> for LpcRef {
    #[inline]
    fn from(f: BaseFloat) -> Self {
        Self::Float(LpcFloat::from(f))
    }
}

impl From<LpcIntInner> for LpcRef {
    #[inline]
    fn from(i: LpcIntInner) -> Self {
        Self::Int(LpcInt::from(i))
    }
}

impl From<LpcInt> for LpcRef {
    #[inline]
    fn from(i: LpcInt) -> Self {
        Self::Int(i)
    }
}

impl From<LpcFloat> for LpcRef {
    #[inline]
    fn from(f: LpcFloat) -> Self {
        Self::Float(f)
    }
}

impl From<bool> for LpcRef {
    #[inline]
    fn from(b: bool) -> Self {
        Self::Int(LpcInt::from(b))
    }
}

impl Hash for LpcRef {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LpcRef::Float(x) => x.hash(state),
            LpcRef::Int(x) => x.hash(state),
            LpcRef::String(x) => x.read().hash(state),
            LpcRef::Array(x) => ptr::hash(&**x, state),
            LpcRef::Mapping(x) => ptr::hash(&**x, state),
            LpcRef::Object(x) => ptr::hash(&**x, state),
            LpcRef::Function(x) => ptr::hash(&**x, state),
        }
    }
}

impl PartialEq for LpcRef {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LpcRef::Float(x), LpcRef::Float(y)) => x == y,
            (LpcRef::Int(x), LpcRef::Int(y)) => x == y,
            (LpcRef::String(x), LpcRef::String(y)) => *x.read() == *y.read(),
            (LpcRef::Object(x), LpcRef::Object(y)) => ptr::eq(&**x, &**y),
            (LpcRef::Array(x), LpcRef::Array(y)) => ptr::eq(&**x, &**y),
            (LpcRef::Mapping(x), LpcRef::Mapping(y)) => ptr::eq(&**x, &**y),
            (LpcRef::Function(x), LpcRef::Function(y)) => ptr::eq(&**x, &**y),
            _ => false,
        }
    }
}

impl Eq for LpcRef {}

impl PartialOrd for LpcRef {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LpcRef::Float(x), LpcRef::Float(y)) => Some(x.cmp(y)),
            (LpcRef::Int(x), LpcRef::Int(y)) => Some(x.cmp(y)),
            (LpcRef::String(x), LpcRef::String(y)) => {
                let a = x.read();
                let b = y.read();
                Some(a.cmp(&*b))
            }
            _ => None,
        }
    }
}

impl Display for LpcRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcRef::Float(x) => write!(f, "{x}"),
            LpcRef::Int(x) => write!(f, "{x}"),
            LpcRef::String(x) => {
                write!(f, "{}", x.read())
            }
            LpcRef::Array(x) => {
                write!(f, "{}", x.read())
            }
            LpcRef::Mapping(x) => {
                write!(f, "{}", x.read())
            }
            LpcRef::Object(x) => match x.upgrade() {
                Some(x) => write!(f, "{}", x),
                None => write!(f, "< destructed >"),
            },
            LpcRef::Function(x) => {
                write!(f, "{}", x)
            }
        }
    }
}

impl Debug for LpcRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcRef::Float(x) => write!(f, "{x:?}"),
            LpcRef::Int(x) => write!(f, "{x:?}"),
            LpcRef::String(x) => {
                write!(f, "{:?}", x.read())
            }
            LpcRef::Array(x) => {
                write!(f, "{:?}", x.read())
            }
            LpcRef::Mapping(x) => {
                write!(f, "{:?}", x.read())
            }
            LpcRef::Object(x) => match x.upgrade() {
                Some(x) => write!(f, "{:?}", x),
                None => write!(f, "< destructed >"),
            },
            LpcRef::Function(x) => {
                write!(f, "{:?}", x)
            }
        }
    }
}

impl Default for LpcRef {
    #[inline]
    fn default() -> Self {
        NULL
    }
}

#[cfg(test)]
mod tests {
    use claims::assert_err;
    use factori::create;

    use super::*;
    use crate::{interpreter::lpc_array::LpcArray, test_support::factories::*};

    mod test_add {
        use indexmap::IndexMap;

        use super::*;

        #[test]
        fn int_int() {
            let int1 = LpcRef::from(123);
            let int2 = LpcRef::from(456);
            let result = int1.add(&int2, &Heap::new(5));
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, 579)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_int_overflow_wraps() {
            let int1 = LpcRef::from(LpcIntInner::MAX);
            let int2 = LpcRef::from(1);
            let result = int1.add(&int2, &Heap::new(5));
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, LpcIntInner::MIN)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn string_string() {
            let pool = Heap::new(5);
            let string1 = LpcString::from("foo").into_lpc_ref(&pool);
            let string2 = LpcString::from("bar").into_lpc_ref(&pool);
            let result = string1.add(&string2, &Heap::new(5));
            if let Ok(LpcRef::String(x)) = result {
                assert_eq!(*x.read(), String::from("foobar"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn string_int() {
            let pool = Heap::new(5);
            let string = LpcString::from("foo").into_lpc_ref(&pool);
            let int = LpcRef::from(123);
            let result = string.add(&int, &pool);
            if let Ok(LpcRef::String(x)) = result {
                assert_eq!(*x.read(), String::from("foo123"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_string() {
            let pool = Heap::new(5);
            let string = LpcString::from("foo").into_lpc_ref(&pool);
            let int = LpcRef::from(123);
            let result = int.add(&string, &Heap::new(5));
            if let Ok(LpcRef::String(x)) = result {
                assert_eq!(*x.read(), String::from("123foo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.add(&int, &Heap::new(5));
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            let float = LpcRef::from(BaseFloat::MAX);
            let int = LpcRef::from(1);
            assert!((float.add(&int, &Heap::new(5))).is_ok());
        }

        #[test]
        fn int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.add(&float, &Heap::new(5));
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt(LpcIntInner::MAX));
            let float = LpcRef::from(1.0);
            assert!((int.add(&float, &Heap::new(5))).is_ok());
        }

        #[test]
        fn array_array() {
            let pool = Heap::new(20);
            let array = LpcArray::new(vec![LpcRef::from(123)]);
            let array2 = LpcArray::new(vec![LpcRef::from(4433)]);
            let result = array
                .clone()
                .into_lpc_ref(&pool)
                .add(&array2.into_lpc_ref(&pool), &pool);

            match &result {
                Ok(v) => {
                    if let LpcRef::Array(a) = v {
                        let a = a.read();
                        assert_ne!(&*a, &array); // ensure the addition makes a fully new copy
                        assert_eq!(&*a, &vec![LpcRef::from(123), LpcRef::from(4433)]);
                    } else {
                        panic!("no match")
                    }
                }
                _ => panic!("no match"),
            }
        }

        #[test]
        fn mapping_mapping() {
            let pool = Heap::new(10);
            let key1 = LpcString::from("key1").into_lpc_ref(&pool);
            let value1 = LpcString::from("value1").into_lpc_ref(&pool);
            let key2 = LpcString::from("key2").into_lpc_ref(&pool);
            let value2 = LpcRef::from(666);

            let mut hash1 = IndexMap::new();
            hash1.insert(key1.clone(), value1.clone());

            let mut hash2 = IndexMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = LpcMapping::new(hash1).into_lpc_ref(&pool);
            let map2 = LpcMapping::new(hash2).into_lpc_ref(&pool);

            let result = map.add(&map2, &Heap::new(5));

            let mut expected = IndexMap::new();
            expected.insert(key1, value1);
            expected.insert(key2, value2);

            if let Ok(LpcRef::Mapping(m)) = result {
                assert_eq!(*m.read(), expected)
            } else {
                panic!("no match. received: {result:?}")
            }
        }

        #[test]
        fn mapping_mapping_duplicate_keys() {
            let pool = Heap::new(20);
            let key1 = LpcString::from("key").into_lpc_ref(&pool);
            let value1 = LpcString::from("value1").into_lpc_ref(&pool);
            let key2 = LpcString::from("key").into_lpc_ref(&pool);
            let value2 = LpcRef::from(666);

            let mut hash1 = IndexMap::new();
            hash1.insert(key1, value1);

            let mut hash2 = IndexMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = LpcMapping::new(hash1).into_lpc_ref(&pool);
            let map2 = LpcMapping::new(hash2).into_lpc_ref(&pool);

            let result = map.add(&map2, &Heap::new(5));

            let mut expected = IndexMap::new();
            expected.insert(key2, value2);

            if let Ok(LpcRef::Mapping(m)) = result {
                assert_eq!(*m.read(), expected)
            } else {
                panic!("no match. received: {result:?}")
            }
        }

        #[test]
        fn add_mismatched() {
            let pool = Heap::new(5);
            let int = LpcRef::from(123);
            let array = LpcArray::new(vec![]).into_lpc_ref(&pool);
            let result = int.add(&array, &pool);

            assert!(result.is_err());
        }
    }

    mod test_sub {
        use super::*;

        #[test]
        fn int_int_underflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt(LpcIntInner::MIN));
            let int2 = LpcRef::from(1);
            let result = int.sub(&int2, &Heap::new(5));
            assert!(result.is_ok());
        }

        #[test]
        fn float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.sub(&int, &Heap::new(5));
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_underflow_does_not_panic() {
            let float = LpcRef::from(BaseFloat::MIN);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = float.sub(&int, &Heap::new(5));
            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.sub(&float, &Heap::new(5));
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, -543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_underflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt::MIN);
            let float = LpcRef::from(1.0);
            let result = int.sub(&float, &Heap::new(5));
            assert!(result.is_ok());
        }

        #[test]
        fn array_array() {
            let pool = Heap::new(10);
            let to_ref = |i| LpcRef::Int(LpcInt(i));
            let v1 = vec![1, 2, 3, 4, 5, 2, 4, 4, 4]
                .into_iter()
                .map(to_ref)
                .collect::<Vec<_>>();
            let v2 = vec![2, 4].into_iter().map(to_ref).collect::<Vec<_>>();
            let a1 = LpcArray::new(v1).into_lpc_ref(&pool);
            let a2 = LpcArray::new(v2).into_lpc_ref(&pool);

            let result = a1.sub(&a2, &pool);
            let expected = vec![1, 3, 5].into_iter().map(to_ref).collect::<Vec<_>>();

            if let Ok(LpcRef::Array(x)) = result {
                assert_eq!(*x.read(), expected)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_mul {
        use super::*;

        #[test]
        fn int_int_overflow_does_not_panic() {
            let pool = Heap::new(5);
            let int = LpcRef::Int(LpcInt::MAX);
            let int2 = LpcRef::from(2);
            let result = int.mul(&int2, &pool);
            assert!(result.is_ok());
        }

        #[test]
        fn string_int() {
            let pool = Heap::new(5);
            let string = LpcString::from("foo").into_lpc_ref(&pool);
            let int = LpcRef::from(4);
            let result = string.mul(&int, &Heap::new(5));
            if let Ok(LpcRef::String(x)) = result {
                assert_eq!(*x.read(), String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_string() {
            let pool = Heap::new(5);
            let string = LpcString::from("foo").into_lpc_ref(&pool);
            let int = LpcRef::from(4);
            let result = int.mul(&string, &Heap::new(5));
            if let Ok(LpcRef::String(x)) = result {
                assert_eq!(*x.read(), String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn string_int_overflow_does_not_panic() {
            let pool = Heap::new(5);
            let string = LpcString::from("1234567890abcdef").into_lpc_ref(&pool);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = string.mul(&int, &pool);
            assert_err!(result.clone());
            assert_eq!(
                result.unwrap_err().to_string().as_str(),
                "overflow in string repetition"
            )
        }

        #[test]
        fn float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.mul(&int, &Heap::new(5));
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            let float = LpcRef::from(BaseFloat::MAX);
            let int = LpcRef::from(2);
            let result = float.mul(&int, &Heap::new(5));
            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.mul(&float, &Heap::new(5));
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt::MAX);
            let float = LpcRef::from(200.0);
            let result = int.mul(&float, &Heap::new(5));
            assert!(result.is_ok());
        }
    }

    mod test_div {
        use super::*;

        #[test]
        fn int_int_overflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt(-1));
            let int2 = LpcRef::Int(LpcInt::MAX);
            let result = int.div(&int2);
            assert!(result.is_ok());
        }

        #[test]
        fn float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.div(&int);

            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 5.42)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            // I'm not sure it's possible to cause an overflow here?
            let float = LpcRef::from(-1.0);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = float.div(&int);

            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.div(&float);

            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 0.18450184501845018)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt(-1));
            let float = LpcRef::from(BaseFloat::MAX);
            let result = int.div(&float);

            assert!(result.is_ok());
        }

        #[test]
        fn div_by_zero() {
            let int = LpcRef::from(123);

            assert!((int.div(&NULL)).is_err());
        }
    }

    mod test_mod {
        use super::*;

        #[test]
        fn int_int_overflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt(-1));
            let int2 = LpcRef::Int(LpcInt::MAX);
            let result = int.rem(&int2);
            assert!(result.is_ok());
        }

        #[test]
        fn float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.rem(&int);

            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 51.65999999999997)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            // I'm not sure it's possible to cause an overflow here?
            let float = LpcRef::from(-1.0);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = float.rem(&int);

            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.rem(&float);

            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 123.0)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt(-1));
            let float = LpcRef::from(BaseFloat::MAX);
            let result = int.rem(&float);

            assert!(result.is_ok());
        }

        #[test]
        fn div_by_zero() {
            let int = LpcRef::from(123);

            assert!((int.rem(&NULL)).is_err());
        }
    }

    mod test_and {
        use super::*;

        #[test]
        fn int_int() {
            let int = LpcRef::from(8);
            let int2 = LpcRef::from(15);
            let result = int.bitand(&int2);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, 8)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_or {
        use super::*;

        #[test]
        fn int_int() {
            let int = LpcRef::from(7);
            let int2 = LpcRef::from(16);
            let result = int.bitor(&int2);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, 23)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_xor {
        use super::*;

        #[test]
        fn int_int() {
            let int = LpcRef::from(7);
            let int2 = LpcRef::from(15);
            let result = int.bitxor(&int2);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, 8)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_shl {
        use super::*;

        #[test]
        fn int_int() {
            let int = LpcRef::from(12345);
            let int2 = LpcRef::from(6);
            let result = int.shl(&int2);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, 790_080)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_shr {
        use super::*;

        #[test]
        fn int_int() {
            let int = LpcRef::from(12345);
            let int2 = LpcRef::from(6);
            let result = int.shr(&int2);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, 192)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_bitwise_not {
        use super::*;

        #[test]
        fn int_int() {
            let int = LpcRef::from(12345);
            let result = int.bitnot();
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, -12346) // one's complement
            } else {
                panic!("no match")
            }
        }
    }

    mod test_inc {
        use super::*;

        #[test]
        fn increments_ints() {
            let mut int = LpcRef::from(123);
            let _ = int.inc();
            assert_eq!(int, LpcRef::from(124))
        }

        #[test]
        fn wraps_on_max() {
            let mut int = LpcRef::Int(LpcInt::MAX);
            let _ = int.inc();
            assert_eq!(int, LpcRef::Int(LpcInt::MIN))
        }

        #[test]
        fn fails_other_types() {
            let pool = Heap::new(5);
            let mut string = LpcString::from("foobar").into_lpc_ref(&pool);
            assert_err!(string.inc());
        }
    }

    mod test_dec {
        use super::*;

        #[test]
        fn decrements_ints() {
            let mut int = LpcRef::from(123);
            let _ = int.dec();
            assert_eq!(int, LpcRef::from(122))
        }

        #[test]
        fn wraps_on_min() {
            let mut int = LpcRef::Int(LpcInt::MIN);
            let _ = int.dec();
            assert_eq!(int, LpcRef::Int(LpcInt::MAX))
        }

        #[test]
        fn fails_other_types() {
            let pool = Heap::new(5);
            let mut string = LpcString::from("foobar").into_lpc_ref(&pool);
            assert_err!(string.dec());
        }
    }

    mod test_mark {
        use indexmap::IndexMap;

        use super::*;
        use crate::interpreter::lpc_mapping::LpcMapping;

        #[test]
        fn test_array() {
            let pool = Heap::new(5);
            let array = LpcArray::new(vec![LpcRef::from(1), LpcRef::from(2), LpcRef::from(3)]);
            let array_id = array.unique_id;
            let array = array.into_lpc_ref(&pool);

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();

            array.mark(&mut marked, &mut processed).unwrap();

            assert!(processed.contains(*array_id.as_ref() as usize));
        }

        #[test]
        fn test_mapping() {
            let pool = Heap::new(5);
            let mapping = LpcMapping::new(IndexMap::new());
            let mapping_id = mapping.unique_id;
            let mapping = mapping.into_lpc_ref(&pool);

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();

            mapping.mark(&mut marked, &mut processed).unwrap();

            assert!(processed.contains(*mapping_id.as_ref() as usize));
        }

        #[test]
        fn test_function() {
            let pool = Heap::new(5);

            let ptr = create!(FunctionPtr);
            let ptr_id = ptr.unique_id;
            let ptr = ptr.into_lpc_ref(&pool);

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();

            ptr.mark(&mut marked, &mut processed).unwrap();

            assert!(processed.contains(*ptr_id.as_ref() as usize));
        }
    }
}
