use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{hash_map::DefaultHasher, HashSet},
    fmt,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    ptr,
};

use bit_set::BitSet;
use lpc_rs_core::{BaseFloat, lpc_type::LpcType, LpcFloat, LpcInt};
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::{string, string::concatenate_strings};
use qcell::{QCell, QCellOwner};
use refpool::PoolRef;
use tracing::{instrument, trace};

use crate::{
    compiler::ast::{binary_op_node::BinaryOperation, unary_op_node::UnaryOperation},
    interpreter::{
        gc::unique_id::UniqueId,
        lpc_value::LpcValue,
    },
    try_extract_value,
    util::keyable::Keyable,
};
use crate::interpreter::gc::mark::Mark;

pub const NULL: LpcRef = LpcRef::Int(0);

/// Convert an LpcValue into an LpcRef, wrapping heap values as necessary
///
/// # Arguments
/// `r` - The expression to be wrapped.
/// `m` - The memory pool to declare the [`PoolRef`]s from.
#[macro_export]
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
            LpcValue::Object(x) => {
                LpcRef::Object(PoolRef::new(&$m, RefCell::new(LpcValue::Object(x))))
            }
            LpcValue::Function(x) => {
                LpcRef::Function(PoolRef::new(&$m, RefCell::new(LpcValue::Function(x))))
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

/// Represent a variable stored in a `Register`. Value types store the actual
/// value. Reference types store a reference to the actual value.
/// This type is intended to be cheap to clone.
#[derive(Eq, Clone)]
pub enum LpcRef {
    Float(LpcFloat),
    Int(LpcInt),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value
    String(PoolRef<RefCell<LpcValue>>),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value
    Array(PoolRef<RefCell<LpcValue>>),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value
    Mapping(PoolRef<RefCell<LpcValue>>),

    /// Reference type, pointing to an LPC `object`
    Object(PoolRef<RefCell<LpcValue>>),

    /// Reference type, a function pointer or closure
    Function(PoolRef<RefCell<LpcValue>>),
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

    fn to_error(&self, op: BinaryOperation, right: &LpcRef, cell_key: &QCellOwner) -> LpcError {
        LpcError::new(format!(
            "runtime error: mismatched types: {} ({}) {} {} ({})",
            self.with_key(cell_key),
            self.type_name(),
            op,
            right.with_key(cell_key),
            right.type_name()
        ))
    }

    fn to_unary_op_error(&self, op: UnaryOperation, cell_key: &QCellOwner) -> LpcError {
        LpcError::new(format!(
            "runtime error: mismatched types: {} {} ({})",
            op,
            self.with_key(cell_key),
            self.type_name()
        ))
    }

    pub fn inc(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(ref mut x) => {
                *x = x.wrapping_add(1);

                Ok(())
            }
            _ => Err(LpcError::new(
                "runtime error: invalid increment".to_string(),
            )),
        }
    }

    pub fn dec(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(ref mut x) => {
                *x = x.wrapping_sub(1);
                Ok(())
            }
            _ => Err(LpcError::new(
                "runtime error: invalid decrement".to_string(),
            )),
        }
    }

    pub fn as_lpc_type(&self) -> LpcType {
        match self {
            LpcRef::Float(_) => LpcType::Float(false),
            LpcRef::Int(_) => LpcType::Int(false),
            LpcRef::String(_) => LpcType::String(false),
            LpcRef::Array(_) => LpcType::Mixed(true), // TODO: this could be better
            LpcRef::Mapping(_) => LpcType::Mapping(false),
            LpcRef::Object(_) => LpcType::Object(false),
            LpcRef::Function(_) => LpcType::Function(false),
        }
    }

    /// Convert this [`LpcRef`] into a [`HashedLpcRef`], for use in a
    /// [`HashMap`](std::collections::HashMap).
    pub fn into_hashed(self, cell_key: &QCellOwner) -> HashedLpcRef {
        HashedLpcRef::new(self, cell_key)
    }

    /// Convenience to perform a binary operation on a pair of [`LpcRef`]s
    /// wrapped in QCells.
    pub fn binary_op<F, T>(
        left: &QCell<Self>,
        right: &QCell<Self>,
        cell_key: &QCellOwner,
        op: F,
    ) -> T
    where
        F: Fn(&LpcRef, &LpcRef) -> T,
    {
        let left = cell_key.ro(left);
        let right = cell_key.ro(right);

        op(left, right)
    }

    pub fn add(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match self {
            LpcRef::Float(f) => match rhs {
                LpcRef::Float(f2) => Ok(LpcValue::Float(*f + *f2)),
                LpcRef::Int(i) => Ok(LpcValue::Float(*f + *i as BaseFloat)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs, cell_key)),
            },
            LpcRef::Int(i) => match rhs {
                LpcRef::Float(f) => Ok(LpcValue::Float(LpcFloat::from(*i as BaseFloat) + *f)),
                LpcRef::Int(i2) => Ok(LpcValue::Int(i.wrapping_add(*i2))),
                LpcRef::String(s) => Ok(LpcValue::String(concatenate_strings(
                    i.to_string(),
                    try_extract_value!(*s.borrow(), LpcValue::String),
                )?)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs, cell_key)),
            },
            LpcRef::String(s) => match rhs {
                LpcRef::String(s2) => Ok(LpcValue::String(concatenate_strings(
                    try_extract_value!(*s.borrow(), LpcValue::String),
                    try_extract_value!(*s2.borrow(), LpcValue::String),
                )?)),
                LpcRef::Int(i) => Ok(LpcValue::String(concatenate_strings(
                    try_extract_value!(*s.borrow(), LpcValue::String),
                    i.to_string(),
                )?)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs, cell_key)),
            },
            LpcRef::Array(vec) => match rhs {
                LpcRef::Array(vec2) => {
                    let mut new_vec = try_extract_value!(*vec.borrow(), LpcValue::Array).clone();
                    let added_vec = try_extract_value!(*vec2.borrow(), LpcValue::Array).clone();
                    new_vec.extend(added_vec.into_iter());
                    Ok(LpcValue::Array(new_vec))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs, cell_key)),
            },
            LpcRef::Mapping(map) => match rhs {
                LpcRef::Mapping(map2) => {
                    let mut new_map = try_extract_value!(*map.borrow(), LpcValue::Mapping).clone();
                    let added_map = try_extract_value!(*map2.borrow(), LpcValue::Mapping).clone();
                    new_map.extend(added_map.into_iter());
                    Ok(LpcValue::Mapping(new_map))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs, cell_key)),
            },
            LpcRef::Object(_) | LpcRef::Function(_) => {
                Err(self.to_error(BinaryOperation::Add, rhs, cell_key))
            }
        }
    }

    pub fn sub(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcValue::Int(x.wrapping_sub(*y))),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(LpcValue::Float(*x - *y)),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(LpcValue::Float(*x - *y as BaseFloat)),
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                Ok(LpcValue::Float(LpcFloat::from(*x as BaseFloat) - *y))
            }
            (LpcRef::Array(vec), LpcRef::Array(vec2)) => {
                let new_vec = try_extract_value!(*vec.borrow(), LpcValue::Array).clone();
                let removed_vec = try_extract_value!(*vec2.borrow(), LpcValue::Array).clone();

                let result = new_vec
                    .into_iter()
                    .filter(|x| !removed_vec.contains(x))
                    .collect();
                Ok(LpcValue::Array(result))
            }
            _ => Err(self.to_error(BinaryOperation::Sub, rhs, cell_key)),
        }
    }

    pub fn mul(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcValue::Int(x.wrapping_mul(*y))),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(LpcValue::Float(*x * *y)),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(LpcValue::Float(*x * *y as BaseFloat)),
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                Ok(LpcValue::Float(LpcFloat::from(*x as BaseFloat) * *y))
            }
            (LpcRef::String(x), LpcRef::Int(y)) => {
                let b = x.borrow();
                let string = try_extract_value!(*b, LpcValue::String);
                Ok(LpcValue::String(string::repeat_string(string, *y)?))
            }
            (LpcRef::Int(x), LpcRef::String(y)) => {
                let b = y.borrow();
                let string = try_extract_value!(*b, LpcValue::String);
                Ok(LpcValue::String(string::repeat_string(string, *x)?))
            }
            _ => Err(self.to_error(BinaryOperation::Mul, rhs, cell_key)),
        }
    }

    pub fn div(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                if y == &0 {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Int(x.wrapping_div(*y)))
                }
            }
            (LpcRef::Float(x), LpcRef::Float(y)) => {
                if (*y - LpcFloat::from(0.0)).into_inner().abs() < BaseFloat::EPSILON {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Float(*x / *y))
                }
            }
            (LpcRef::Float(x), LpcRef::Int(y)) => {
                if y == &0 {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Float(*x / *y as BaseFloat))
                }
            }
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                if (*y - LpcFloat::from(0.0)).into_inner().abs() < BaseFloat::EPSILON {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Float(LpcFloat::from(*x as BaseFloat) / *y))
                }
            }
            _ => Err(self.to_error(BinaryOperation::Div, rhs, cell_key)),
        }
    }

    pub fn rem(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                if y == &0 {
                    Err(LpcError::new("Runtime Error: Remainder division by zero"))
                } else {
                    Ok(LpcValue::Int(x.wrapping_rem(*y)))
                }
            }
            (LpcRef::Float(x), LpcRef::Float(y)) => {
                if (*y - LpcFloat::from(0.0)).into_inner().abs() < BaseFloat::EPSILON {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Float(*x % *y))
                }
            }
            (LpcRef::Float(x), LpcRef::Int(y)) => {
                if y == &0 {
                    Err(LpcError::new("Runtime Error: Division by zero"))
                } else {
                    Ok(LpcValue::Float(*x % *y as BaseFloat))
                }
            }
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                if (*y - LpcFloat::from(0.0)).into_inner().abs() < BaseFloat::EPSILON {
                    Err(LpcError::new("Runtime Error: Remainder division by zero"))
                } else {
                    Ok(LpcValue::Float(LpcFloat::from(*x as BaseFloat) % *y))
                }
            }
            _ => Err(self.to_error(BinaryOperation::Mod, rhs, cell_key)),
        }
    }

    pub fn bitand(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcValue::Int(*x & *y)),
            _ => Err(self.to_error(BinaryOperation::And, rhs, cell_key)),
        }
    }

    pub fn bitor(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcValue::Int(*x | *y)),
            _ => Err(self.to_error(BinaryOperation::Or, rhs, cell_key)),
        }
    }

    pub fn bitxor(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(LpcValue::Int(*x ^ *y)),
            _ => Err(self.to_error(BinaryOperation::Xor, rhs, cell_key)),
        }
    }

    pub fn shl(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                let modulo: LpcInt = y % (LpcInt::BITS as LpcInt);

                let shift_by: u32 = if modulo < 0 {
                    LpcInt::BITS - (modulo.unsigned_abs() as u32)
                } else {
                    modulo as u32
                };

                Ok(LpcValue::Int(x.checked_shl(shift_by).unwrap_or(0)))
            }
            _ => Err(self.to_error(BinaryOperation::Shl, rhs, cell_key)),
        }
    }

    pub fn shr(&self, rhs: &Self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => {
                let modulo: LpcInt = y % (LpcInt::BITS as LpcInt);

                let shift_by: u32 = if modulo < 0 {
                    LpcInt::BITS - (modulo.unsigned_abs() as u32)
                } else {
                    modulo as u32
                };

                Ok(LpcValue::Int(x.checked_shr(shift_by).unwrap_or(0)))
            }
            _ => Err(self.to_error(BinaryOperation::Shr, rhs, cell_key)),
        }
    }

    /// Impl _bitwise_ Not for ints, (i.e. the unary `~` operator)
    pub fn bitnot(&self, cell_key: &QCellOwner) -> Result<LpcValue> {
        match &self {
            LpcRef::Int(x) => Ok(LpcValue::Int(!*x)),
            _ => Err(self.to_unary_op_error(UnaryOperation::BitwiseNot, cell_key)),
        }
    }
}

impl Mark for LpcRef {
    #[instrument(skip(self, cell_key))]
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut HashSet<UniqueId>,
        cell_key: &QCellOwner,
    ) -> Result<()> {
        trace!("marking lpc ref of {type}", type = self.as_lpc_type());

        match self {
            LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::String(_) | LpcRef::Object(_) => Ok(()),
            LpcRef::Array(arr) => {
                let arr = arr.borrow();
                let arr = try_extract_value!(&*arr, LpcValue::Array);
                arr.mark(marked, processed, cell_key)
            }
            LpcRef::Mapping(map) => {
                let map = map.borrow();
                let map = try_extract_value!(&*map, LpcValue::Mapping);
                map.mark(marked, processed, cell_key)
            }
            LpcRef::Function(fun) => {
                let fun = fun.borrow();
                let fun = try_extract_value!(&*fun, LpcValue::Function);

                fun.mark(marked, processed, cell_key)
            }
        }
    }
}

impl From<BaseFloat> for LpcRef {
    fn from(f: BaseFloat) -> Self {
        Self::Float(LpcFloat::from(f))
    }
}

impl Hash for LpcRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LpcRef::Float(x) => x.hash(state),
            LpcRef::Int(x) => x.hash(state),
            LpcRef::String(x) => extract_value!(*x.borrow(), LpcValue::String).hash(state),
            LpcRef::Array(x) => ptr::hash(&**x, state),
            LpcRef::Mapping(x) => ptr::hash(&**x, state),
            LpcRef::Object(x) => ptr::hash(&**x, state),
            LpcRef::Function(x) => ptr::hash(&**x, state),
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
            (LpcRef::Array(x), LpcRef::Array(y)) => PoolRef::ptr_eq(x, y),
            (LpcRef::Mapping(x), LpcRef::Mapping(y)) => PoolRef::ptr_eq(x, y),
            (LpcRef::Function(x), LpcRef::Function(y)) => PoolRef::ptr_eq(x, y),
            _ => false,
        }
    }
}

impl PartialOrd for LpcRef {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LpcRef::Float(x), LpcRef::Float(y)) => Some(x.cmp(y)),
            (LpcRef::Int(x), LpcRef::Int(y)) => Some(x.cmp(y)),
            (LpcRef::String(x), LpcRef::String(y)) => {
                let xb = x.borrow();
                let yb = y.borrow();
                let a = extract_value!(*xb, LpcValue::String);
                let b = extract_value!(*yb, LpcValue::String);
                Some(a.cmp(b))
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
            LpcRef::String(x)
            | LpcRef::Array(x)
            | LpcRef::Mapping(x)
            | LpcRef::Object(x)
            | LpcRef::Function(x) => {
                write!(f, "{}", x.borrow())
            }
        }
    }
}

impl Debug for LpcRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcRef::Float(x) => write!(f, "{x}"),
            LpcRef::Int(x) => write!(f, "{x}"),
            LpcRef::String(x)
            | LpcRef::Array(x)
            | LpcRef::Mapping(x)
            | LpcRef::Object(x)
            | LpcRef::Function(x) => {
                write!(f, "{:?}", x.borrow())
            }
        }
    }
}

impl Default for LpcRef {
    fn default() -> Self {
        NULL
    }
}

impl<'a> Keyable<'a> for LpcRef {
    fn keyable_debug(&self, f: &mut Formatter<'_>, _cell_key: &QCellOwner) -> fmt::Result {
        write!(f, "{:?}", self)
    }

    fn keyable_display(&self, f: &mut Formatter<'_>, _cell_key: &QCellOwner) -> fmt::Result {
        write!(f, "{}", self)
    }

    fn keyable_hash<H: Hasher>(&self, state: &mut H, _cell_key: &QCellOwner) {
        self.hash(state)
    }

    fn keyable_eq(&self, other: &Self, _cell_key: &QCellOwner) -> bool {
        self.eq(other)
    }

    fn keyable_partial_cmp(&self, other: &Self, _cell_key: &QCellOwner) -> Option<Ordering> {
        self.partial_cmp(other)
    }
}

/// A structure that contains a pre-calculated hash, as LpcRef requires access
/// to [`QCell`]s to calculate the hash.
#[derive(Clone, Debug)]
#[readonly::make]
pub struct HashedLpcRef {
    pub hash: u64,
    pub value: LpcRef,
}

impl HashedLpcRef {
    pub fn new(value: LpcRef, _cell_key: &QCellOwner) -> Self {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        Self {
            hash: hasher.finish(),
            value,
        }
    }
}

impl Hash for HashedLpcRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state)
    }
}

impl PartialEq for HashedLpcRef {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}

impl Eq for HashedLpcRef {}

impl<'a> Keyable<'a> for HashedLpcRef {
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> fmt::Result {
        self.value.keyable_debug(f, cell_key)
    }

    fn keyable_display(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> fmt::Result {
        self.value.keyable_display(f, cell_key)
    }

    fn keyable_hash<H: Hasher>(&self, state: &mut H, cell_key: &QCellOwner) {
        self.value.keyable_hash(state, cell_key)
    }

    fn keyable_eq(&self, other: &Self, cell_key: &QCellOwner) -> bool {
        self.value.keyable_eq(&other.value, cell_key)
    }

    fn keyable_partial_cmp(&self, other: &Self, cell_key: &QCellOwner) -> Option<Ordering> {
        self.value.keyable_partial_cmp(&other.value, cell_key)
    }
}

#[cfg(test)]
mod tests {
    use claim::assert_err;
    use refpool::Pool;

    use super::*;
    use crate::interpreter::lpc_array::LpcArray;

    mod test_add {
        use indexmap::IndexMap;

        use super::*;

        #[test]
        fn int_int() {
            let cell_key = QCellOwner::new();
            let int1 = LpcRef::Int(123);
            let int2 = LpcRef::Int(456);
            let result = int1.add(&int2, &cell_key);
            if let Ok(LpcValue::Int(x)) = result {
                assert_eq!(x, 579)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_int_overflow_wraps() {
            let cell_key = QCellOwner::new();
            let int1 = LpcRef::Int(LpcInt::MAX);
            let int2 = LpcRef::Int(1);
            let result = int1.add(&int2, &cell_key);
            if let Ok(LpcValue::Int(x)) = result {
                assert_eq!(x, LpcInt::MIN)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn string_string() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(20);
            let string1 = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let string2 = value_to_ref!(LpcValue::String("bar".to_string()), pool);
            let result = string1.add(&string2, &cell_key);
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foobar"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn string_int() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let int = LpcRef::Int(123);
            let result = string.add(&int, &cell_key);
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foo123"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_string() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let int = LpcRef::Int(123);
            let result = int.add(&string, &cell_key);
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("123foo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = float.add(&int, &cell_key);
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(BaseFloat::MAX);
            let int = LpcRef::Int(1);
            assert!((float.add(&int, &cell_key)).is_ok());
        }

        #[test]
        fn int_float() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = int.add(&float, &cell_key);
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(LpcInt::MAX);
            let float = LpcRef::from(1.0);
            assert!((int.add(&float, &cell_key)).is_ok());
        }

        #[test]
        fn array_array() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(20);
            let array = LpcValue::from(vec![LpcRef::Int(123)]);
            let array2 = LpcValue::from(vec![LpcRef::Int(4433)]);
            let result =
                value_to_ref!(array.clone(), pool).add(&value_to_ref!(array2, pool), &cell_key);

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
        fn mapping_mapping() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(20);
            let key1 = value_to_ref!(LpcValue::from("key1"), pool);
            let value1 = value_to_ref!(LpcValue::from("value1"), pool);
            let key2 = value_to_ref!(LpcValue::from("key2"), pool);
            let value2 = value_to_ref!(LpcValue::from(666), pool);

            let mut hash1 = IndexMap::new();
            hash1.insert(key1.clone().into_hashed(&cell_key), value1.clone());

            let mut hash2 = IndexMap::new();
            hash2.insert(key2.clone().into_hashed(&cell_key), value2.clone());

            let map = value_to_ref!(LpcValue::from(hash1), pool);
            let map2 = value_to_ref!(LpcValue::from(hash2), pool);

            let result = map.add(&map2, &cell_key);

            let mut expected = IndexMap::new();
            expected.insert(key1.into_hashed(&cell_key), value1);
            expected.insert(key2.into_hashed(&cell_key), value2);

            if let Ok(LpcValue::Mapping(m)) = result {
                assert_eq!(m, expected)
            } else {
                panic!("no match. received: {result:?}")
            }
        }

        #[test]
        fn mapping_mapping_duplicate_keys() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(20);
            let key1 = value_to_ref!(LpcValue::from("key"), pool);
            let value1 = value_to_ref!(LpcValue::from("value1"), pool);
            let key2 = value_to_ref!(LpcValue::from("key"), pool);
            let value2 = value_to_ref!(LpcValue::from(666), pool);

            let mut hash1 = IndexMap::new();
            hash1.insert(key1.into_hashed(&cell_key), value1);

            let mut hash2 = IndexMap::new();
            hash2.insert(key2.clone().into_hashed(&cell_key), value2.clone());

            let map = value_to_ref!(LpcValue::from(hash1), pool);
            let map2 = value_to_ref!(LpcValue::from(hash2), pool);

            let result = map.add(&map2, &cell_key);

            let mut expected = IndexMap::new();
            expected.insert(key2.into_hashed(&cell_key), value2);

            if let Ok(LpcValue::Mapping(m)) = result {
                assert_eq!(m, expected)
            } else {
                panic!("no match. received: {result:?}")
            }
        }

        #[test]
        fn add_mismatched() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(5);
            let int = LpcRef::Int(123);
            let array = value_to_ref!(LpcValue::Array(LpcArray::new(vec![])), pool);
            let result = int.add(&array, &cell_key);

            assert!(result.is_err());
        }
    }

    mod test_sub {
        use super::*;

        #[test]
        fn int_int_underflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(LpcInt::MIN);
            let int2 = LpcRef::Int(1);
            let result = int.sub(&int2, &cell_key);
            assert!(result.is_ok());
        }

        #[test]
        fn float_int() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = float.sub(&int, &cell_key);
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_underflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(BaseFloat::MIN);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = float.sub(&int, &cell_key);
            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = int.sub(&float, &cell_key);
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, -543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_underflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(LpcInt::MIN);
            let float = LpcRef::from(1.0);
            let result = int.sub(&float, &cell_key);
            assert!(result.is_ok());
        }

        #[test]
        fn array_array() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(10);
            let to_ref = LpcRef::Int;
            let v1 = vec![1, 2, 3, 4, 5, 2, 4, 4, 4]
                .into_iter()
                .map(to_ref)
                .collect::<Vec<_>>();
            let v2 = vec![2, 4].into_iter().map(to_ref).collect::<Vec<_>>();
            let a1 = value_to_ref!(LpcValue::from(v1), pool);
            let a2 = value_to_ref!(LpcValue::from(v2), pool);

            let result = a1.sub(&a2, &cell_key);
            let expected = vec![1, 3, 5].into_iter().map(to_ref).collect::<Vec<_>>();

            if let Ok(LpcValue::Array(x)) = result {
                assert_eq!(x, expected)
            } else {
                panic!("no match")
            }
        }
    }

    mod test_mul {
        use super::*;

        #[test]
        fn int_int_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(LpcInt::MAX);
            let int2 = LpcRef::Int(2);
            let result = int.mul(&int2, &cell_key);
            assert!(result.is_ok());
        }

        #[test]
        fn string_int() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let int = LpcRef::Int(4);
            let result = string.mul(&int, &cell_key);
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_string() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("foo".to_string()), pool);
            let int = LpcRef::Int(4);
            let result = int.mul(&string, &cell_key);
            if let Ok(LpcValue::String(x)) = result {
                assert_eq!(x, String::from("foofoofoofoo"))
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn string_int_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let pool = Pool::new(5);
            let string = value_to_ref!(LpcValue::String("1234567890abcdef".to_string()), pool);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = string.mul(&int, &cell_key);
            assert_err!(result.clone());
            assert_eq!(
                result.unwrap_err().to_string().as_str(),
                "overflow in string repetition"
            )
        }

        #[test]
        fn float_int() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = float.mul(&int, &cell_key);
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(BaseFloat::MAX);
            let int = LpcRef::Int(2);
            let result = float.mul(&int, &cell_key);
            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = int.mul(&float, &cell_key);
            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 81999.18)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(LpcInt::MAX);
            let float = LpcRef::from(200.0);
            let result = int.mul(&float, &cell_key);
            assert!(result.is_ok());
        }
    }

    mod test_div {
        use super::*;

        #[test]
        fn int_int_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(-1);
            let int2 = LpcRef::Int(LpcInt::MAX);
            let result = int.div(&int2, &cell_key);
            assert!(result.is_ok());
        }

        #[test]
        fn float_int() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = float.div(&int, &cell_key);

            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 5.42)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            // I'm not sure it's possible to cause an overflow here?
            let float = LpcRef::from(-1.0);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = float.div(&int, &cell_key);

            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = int.div(&float, &cell_key);

            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 0.18450184501845018)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(-1);
            let float = LpcRef::from(BaseFloat::MAX);
            let result = int.div(&float, &cell_key);

            assert!(result.is_ok());
        }

        #[test]
        fn div_by_zero() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(123);

            assert!((int.div(&NULL, &cell_key)).is_err());
        }
    }

    mod test_mod {
        use super::*;

        #[test]
        fn int_int_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(-1);
            let int2 = LpcRef::Int(LpcInt::MAX);
            let result = int.rem(&int2, &cell_key);
            assert!(result.is_ok());
        }

        #[test]
        fn float_int() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = float.rem(&int, &cell_key);

            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 51.65999999999997)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            // I'm not sure it's possible to cause an overflow here?
            let float = LpcRef::from(-1.0);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = float.rem(&int, &cell_key);

            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let cell_key = QCellOwner::new();
            let float = LpcRef::from(666.66);
            let int = LpcRef::Int(123);
            let result = int.rem(&float, &cell_key);

            if let Ok(LpcValue::Float(x)) = result {
                assert_eq!(x, 123.0)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(-1);
            let float = LpcRef::from(BaseFloat::MAX);
            let result = int.rem(&float, &cell_key);

            assert!(result.is_ok());
        }

        #[test]
        fn div_by_zero() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(123);

            assert!((int.div(&NULL, &cell_key)).is_err());
        }
    }

    mod test_and {
        use super::*;

        #[test]
        fn int_int() {
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(8);
            let int2 = LpcRef::Int(15);
            let result = int.bitand(&int2, &cell_key);
            if let Ok(LpcValue::Int(x)) = result {
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
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(7);
            let int2 = LpcRef::Int(16);
            let result = int.bitor(&int2, &cell_key);
            if let Ok(LpcValue::Int(x)) = result {
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
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(7);
            let int2 = LpcRef::Int(15);
            let result = int.bitxor(&int2, &cell_key);
            if let Ok(LpcValue::Int(x)) = result {
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
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(12345);
            let int2 = LpcRef::Int(6);
            let result = int.shl(&int2, &cell_key);
            if let Ok(LpcValue::Int(x)) = result {
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
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(12345);
            let int2 = LpcRef::Int(6);
            let result = int.shr(&int2, &cell_key);
            if let Ok(LpcValue::Int(x)) = result {
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
            let cell_key = QCellOwner::new();
            let int = LpcRef::Int(12345);
            let result = int.bitnot(&cell_key);
            if let Ok(LpcValue::Int(x)) = result {
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
            let mut int = LpcRef::Int(123);
            let _ = int.inc();
            assert_eq!(int, LpcRef::Int(124))
        }

        #[test]
        fn wraps_on_max() {
            let mut int = LpcRef::Int(LpcInt::MAX);
            let _ = int.inc();
            assert_eq!(int, LpcRef::Int(LpcInt::MIN))
        }

        #[test]
        fn fails_other_types() {
            let pool = Pool::new(5);
            let mut string = value_to_ref!(LpcValue::String("foobar".to_string()), pool);
            assert_err!(string.inc());
        }
    }

    mod test_dec {
        use super::*;

        #[test]
        fn decrements_ints() {
            let mut int = LpcRef::Int(123);
            let _ = int.dec();
            assert_eq!(int, LpcRef::Int(122))
        }

        #[test]
        fn wraps_on_min() {
            let mut int = LpcRef::Int(LpcInt::MIN);
            let _ = int.dec();
            assert_eq!(int, LpcRef::Int(LpcInt::MAX))
        }

        #[test]
        fn fails_other_types() {
            let pool = Pool::new(5);
            let mut string = value_to_ref!(LpcValue::String("foobar".to_string()), pool);
            assert_err!(string.dec());
        }
    }
}
