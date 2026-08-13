use std::{
    cmp::Ordering,
    fmt,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    ptr,
    sync::{Arc, Weak},
};

use bit_set::BitSet;
use lpc_rs_core::{BaseFloat, LpcFloatInner, LpcIntInner, lpc_type::LpcType};
use lpc_rs_errors::{LpcError, Result, lpc_error};
use lpc_rs_utils::{string, string::concatenate_strings};
use parking_lot::RwLock;
use tracing::{instrument, trace};

use crate::{
    compiler::ast::{binary_op_node::BinaryOperation, unary_op_node::UnaryOperation},
    interpreter::{
        function_type::function_ptr::FunctionPtr, gc::mark::Mark, lpc_array::LpcArray,
        lpc_float::LpcFloat, lpc_int::LpcInt, lpc_mapping::LpcMapping, lpc_string::LpcString,
        process::Process,
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
    String(Arc<RwLock<LpcString>>),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value
    Array(Arc<RwLock<LpcArray>>),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value
    Mapping(Arc<RwLock<LpcMapping>>),

    /// Reference type, pointing to an LPC `object`
    Object(Weak<Process>),

    /// Reference type, a function pointer or closure
    Function(Arc<FunctionPtr>),
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
            "Runtime Error: mismatched types: {} ({}) {} {} ({})",
            self,
            self.type_name(),
            op,
            right,
            right.type_name()
        )
    }

    fn to_unary_op_error(&self, op: UnaryOperation) -> Box<LpcError> {
        lpc_error!(
            "Runtime Error: mismatched types: {} {} ({})",
            op,
            self,
            self.type_name()
        )
    }

    pub fn inc(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(x) => {
                *x = x.wrapping_add(1).into();

                Ok(())
            }
            _ => Err(lpc_error!("Runtime Error: invalid increment")),
        }
    }

    pub fn dec(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(x) => {
                *x = x.wrapping_sub(1).into();
                Ok(())
            }
            _ => Err(lpc_error!("Runtime Error: invalid decrement")),
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

    pub fn with_string<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(&LpcString) -> R,
    {
        match self {
            LpcRef::String(a) => Ok(f(&a.read())),
            _ => Err(lpc_error!(
                "Runtime Error: invalid access. Expected string, actually {}",
                self.type_name()
            )),
        }
    }

    // pub fn with_string_mut<F, R>(&self, f: F) -> Result<R>
    // where
    //     F: FnOnce(&mut LpcString) -> R
    // {
    //     match self {
    //         LpcRef::String(a) => Ok(f(&mut *a.write())),
    //         _ => Err(lpc_error!("Runtime Error: invalid access. Expected string, actually {}", self.type_name())),
    //     }
    // }

    pub fn with_strings<F, R>(&self, other: &Self, f: F) -> Result<R>
    where
        F: FnOnce(&LpcString, &LpcString) -> R,
    {
        match (self, other) {
            (LpcRef::String(a), LpcRef::String(b)) => {
                if Arc::ptr_eq(a, b) {
                    // Avoid taking a single lock twice
                    let g = a.read();
                    return Ok(f(&g, &g));
                }
                // take locks in a deterministic order (lower address first)
                let (aa, bb) = if Arc::as_ptr(a) < Arc::as_ptr(b) {
                    (a.read(), b.read())
                } else {
                    let bb = b.read();
                    (a.read(), bb)
                };

                Ok(f(&aa, &bb))
            }
            _ => Err(lpc_error!(
                "Runtime Error: invalid access. Expected string / string, actually {} / {}",
                self.type_name(),
                other.type_name()
            )),
        }
    }

    pub fn with_array<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(&LpcArray) -> R,
    {
        match self {
            LpcRef::Array(a) => Ok(f(&a.read())),
            _ => Err(lpc_error!(
                "Runtime Error: invalid access. Expected array, actually {}",
                self.type_name()
            )),
        }
    }

    pub fn with_array_mut<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(&mut LpcArray) -> R,
    {
        match self {
            LpcRef::Array(a) => Ok(f(&mut a.write())),
            _ => Err(lpc_error!(
                "Runtime Error: invalid access. Expected array, actually {}",
                self.type_name()
            )),
        }
    }

    pub fn with_arrays<F, R>(&self, other: &Self, f: F) -> Result<R>
    where
        F: FnOnce(&LpcArray, &LpcArray) -> R,
    {
        match (self, other) {
            (LpcRef::Array(a), LpcRef::Array(b)) => {
                if Arc::ptr_eq(a, b) {
                    let g = a.read();
                    return Ok(f(&g, &g));
                }
                let (aa, bb) = if Arc::as_ptr(a) < Arc::as_ptr(b) {
                    (a.read(), b.read())
                } else {
                    let bb = b.read();
                    (a.read(), bb)
                };

                Ok(f(&aa, &bb))
            }
            _ => Err(lpc_error!(
                "Runtime Error: invalid access. Expected array / array, actually {} / {}",
                self.type_name(),
                other.type_name()
            )),
        }
    }

    pub fn with_mapping<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(&LpcMapping) -> R,
    {
        match self {
            LpcRef::Mapping(m) => Ok(f(&m.read())),
            _ => Err(lpc_error!(
                "Runtime Error: invalid access. Expected mapping, actually {}",
                self.type_name()
            )),
        }
    }

    pub fn with_mapping_mut<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(&mut LpcMapping) -> R,
    {
        match self {
            LpcRef::Mapping(m) => Ok(f(&mut m.write())),
            _ => Err(lpc_error!(
                "Runtime Error: invalid access. Expected mapping, actually {}",
                self.type_name()
            )),
        }
    }

    pub fn with_mappings<F, R>(&self, other: &Self, f: F) -> Result<R>
    where
        F: FnOnce(&LpcMapping, &LpcMapping) -> R,
    {
        match (self, other) {
            (LpcRef::Mapping(a), LpcRef::Mapping(b)) => {
                if Arc::ptr_eq(a, b) {
                    let g = a.read();
                    return Ok(f(&g, &g));
                }
                let (aa, bb) = if Arc::as_ptr(a) < Arc::as_ptr(b) {
                    (a.read(), b.read())
                } else {
                    let bb = b.read();
                    (a.read(), bb)
                };

                Ok(f(&aa, &bb))
            }
            _ => Err(lpc_error!(
                "Runtime Error: invalid access. Expected mapping / mapping, actually {} / {}",
                self.type_name(),
                other.type_name()
            )),
        }
    }

    // /// Convenience to perform a binary operation on a pair of [`LpcRef`]s
    // /// wrapped in RwLocks.
    // pub fn binary_op<F, T>(left: &RwLock<Self>, right: &RwLock<Self>, op: F) -> T
    // where
    //     F: Fn(&LpcRef, &LpcRef) -> T,
    // {
    //     let left = &*left.read();
    //     let right = &*right.read();
    //
    //     op(left, right)
    // }

    pub fn add(&self, rhs: &Self) -> Result<Self> {
        match self {
            LpcRef::Float(f) => match rhs {
                LpcRef::Float(f2) => Ok(Self::from(*f + *f2)),
                LpcRef::Int(i) => Ok(Self::from(*f + *i)),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Int(i) => match rhs {
                LpcRef::Float(f) => Ok(Self::from(*i + *f)),
                LpcRef::Int(i2) => Ok(Self::from(*i + *i2)),
                LpcRef::String(_) => {
                    let result = rhs
                        .with_string(|s| concatenate_strings(i.to_string(), s.to_str()))
                        .flatten()?;
                    Ok(LpcString::from(result).into())
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::String(_) => match rhs {
                LpcRef::String(_) => {
                    let result = self
                        .with_strings(rhs, |a, b| concatenate_strings(a.to_string(), b.to_str()))
                        .flatten()?;
                    Ok(LpcString::from(result).into())
                }
                LpcRef::Int(i) => {
                    let result = self
                        .with_string(|s| concatenate_strings(s.to_string(), i.to_string()))
                        .flatten()?;
                    Ok(LpcString::from(result).into())
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Array(_) => match rhs {
                LpcRef::Array(_) => {
                    self.with_arrays(rhs, |a, b| LpcRef::from(a.clone() + b.clone()))
                }
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Mapping(_) => match rhs {
                LpcRef::Mapping(_) => self.with_mappings(rhs, |map1, map2| {
                    let mut new_map = map1.clone();
                    let added_map = map2.clone();
                    new_map.extend(added_map);
                    new_map.into()
                }),
                _ => Err(self.to_error(BinaryOperation::Add, rhs)),
            },
            LpcRef::Object(_) | LpcRef::Function(_) => {
                Err(self.to_error(BinaryOperation::Add, rhs))
            }
        }
    }

    pub fn sub(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(Self::Int(x.wrapping_sub(y.0).into())),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(Self::Float((x.0 - y.0).into())),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(Self::Float((x.0 - y.0 as BaseFloat).into())),
            (LpcRef::Int(x), LpcRef::Float(y)) => Ok(Self::Float(LpcFloat::from(
                LpcFloatInner::from(x.0 as BaseFloat) - y.0,
            ))),
            (LpcRef::Array(_), LpcRef::Array(_)) => {
                let new_vec = self.with_array(|a| a.clone())?;

                rhs.with_array(|a| {
                    new_vec
                        .into_iter()
                        .filter(|x| !a.contains(x))
                        .collect::<LpcArray>()
                        .into()
                })
            }
            _ => Err(self.to_error(BinaryOperation::Sub, rhs)),
        }
    }

    pub fn mul(&self, rhs: &Self) -> Result<Self> {
        match (&self, &rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(Self::Int(x.0.wrapping_mul(y.0).into())),
            (LpcRef::Float(x), LpcRef::Float(y)) => Ok(Self::Float((x.0 * y.0).into())),
            (LpcRef::Float(x), LpcRef::Int(y)) => Ok(Self::Float((x.0 * y.0 as BaseFloat).into())),
            (LpcRef::Int(x), LpcRef::Float(y)) => Ok(Self::Float(
                (LpcFloatInner::from(x.0 as BaseFloat) * y.0).into(),
            )),
            (LpcRef::String(_), LpcRef::Int(y)) => {
                let string = self
                    .with_string(|a| string::repeat_string(a.to_str(), y.0))
                    .flatten()?;
                Ok(LpcString::from(string).into())
            }
            (LpcRef::Int(x), LpcRef::String(_)) => {
                let string = rhs
                    .with_string(|a| string::repeat_string(a.to_str(), x.0))
                    .flatten()?;
                Ok(LpcString::from(string).into())
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
            LpcRef::Array(_) => self.with_array(|a| a.mark(marked, processed)).flatten(),
            LpcRef::Mapping(_) => self.with_mapping(|m| m.mark(marked, processed)).flatten(),
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

impl From<&str> for LpcRef {
    #[inline]
    fn from(value: &str) -> Self {
        LpcRef::String(Arc::new(RwLock::new(LpcString::from(value))))
    }
}

impl From<String> for LpcRef {
    #[inline]
    fn from(value: String) -> Self {
        LpcRef::String(Arc::new(RwLock::new(LpcString::from(value))))
    }
}

impl From<LpcString> for LpcRef {
    #[inline]
    fn from(value: LpcString) -> Self {
        LpcRef::String(Arc::new(RwLock::new(value)))
    }
}

impl From<LpcArray> for LpcRef {
    #[inline]
    fn from(value: LpcArray) -> Self {
        LpcRef::Array(Arc::new(RwLock::new(value)))
    }
}

impl From<LpcMapping> for LpcRef {
    #[inline]
    fn from(value: LpcMapping) -> Self {
        LpcRef::Mapping(Arc::new(RwLock::new(value)))
    }
}

impl From<FunctionPtr> for LpcRef {
    #[inline]
    fn from(value: FunctionPtr) -> Self {
        LpcRef::Function(Arc::new(value))
    }
}

impl From<Weak<Process>> for LpcRef {
    #[inline]
    fn from(value: Weak<Process>) -> Self {
        LpcRef::Object(value)
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
            LpcRef::String(_) => self.with_string(|s| s.hash(state)).expect("unreachable"),
            LpcRef::Array(x) => ptr::hash(&**x, state),
            LpcRef::Mapping(x) => ptr::hash(&**x, state),
            LpcRef::Object(x) => ptr::hash(x, state),
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
            (LpcRef::String(_), LpcRef::String(_)) => self
                .with_strings(other, |a, b| a == b)
                .expect("Strings should be valid"),
            (LpcRef::Object(x), LpcRef::Object(y)) => ptr::eq(x, y),
            (LpcRef::Array(x), LpcRef::Array(y)) => ptr::eq(&**x, &**y),
            (LpcRef::Mapping(x), LpcRef::Mapping(y)) => ptr::eq(&**x, &**y),
            (LpcRef::Function(x), LpcRef::Function(y)) => ptr::eq(&**x, &**y),
            _ => false,
        }
    }
}

impl Eq for LpcRef {}

impl PartialEq<&Process> for LpcRef {
    fn eq(&self, other: &&Process) -> bool {
        match self {
            LpcRef::Object(x) => {
                let Some(x) = x.upgrade() else {
                    return false;
                };

                ptr::eq(&*x, *other)
            }
            _ => false,
        }
    }
}

impl PartialEq<usize> for LpcRef {
    fn eq(&self, other: &usize) -> bool {
        match self {
            LpcRef::Int(x) => x.0 as usize == *other,
            _ => false,
        }
    }
}

impl PartialOrd for LpcRef {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LpcRef::Float(x), LpcRef::Float(y)) => Some(x.cmp(y)),
            (LpcRef::Int(x), LpcRef::Int(y)) => Some(x.cmp(y)),
            (LpcRef::String(_), LpcRef::String(_)) => {
                self.with_strings(other, |a, b| a.cmp(b)).ok()
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
            LpcRef::String(_) => self
                .with_string(|s| write!(f, "{}", s))
                .expect("Strings should be valid"),
            LpcRef::Array(_) => self
                .with_array(|a| write!(f, "{a}"))
                .expect("Arrays should be valid"),
            LpcRef::Mapping(_) => self
                .with_mapping(|m| write!(f, "{m}"))
                .expect("Mappings should be valid"),
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
            LpcRef::String(_) => self
                .with_string(|x| write!(f, "{x:?}"))
                .expect("Strings should be valid"),
            LpcRef::Array(_) => self
                .with_array(|x| write!(f, "{x:?}"))
                .expect("Arrays should be valid"),
            LpcRef::Mapping(_) => self
                .with_mapping(|x| write!(f, "{x:?}"))
                .expect("Mappings should be valid"),
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
            let result = int1.add(&int2);
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
            let result = int1.add(&int2);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, LpcIntInner::MIN)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn string_string() {
            let string1 = LpcRef::from("foo");
            let string2 = LpcRef::from(LpcString::from("bar"));
            let result = string1.add(&string2).unwrap();
            result.with_string(|x| assert_eq!(x, "foobar")).unwrap();
        }

        #[test]
        fn string_int() {
            let string = LpcRef::from("foo");
            let int = LpcRef::from(123);
            let result = string.add(&int).unwrap();
            result.with_string(|x| assert_eq!(x, "foo123")).unwrap();
        }

        #[test]
        fn int_string() {
            let string = LpcRef::from("foo");
            let int = LpcRef::from(123);
            let result = int.add(&string).unwrap();
            result
                .with_string(|x| {
                    assert_eq!(x, "123foo");
                })
                .unwrap();
        }

        #[test]
        fn float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.add(&int);
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
            assert!((float.add(&int)).is_ok());
        }

        #[test]
        fn int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.add(&float);
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
            assert!((int.add(&float)).is_ok());
        }

        #[test]
        fn array_array() {
            let array = LpcArray::new(vec![LpcRef::from(123)]);
            let array2 = LpcArray::new(vec![LpcRef::from(4433)]);
            let result = LpcRef::from(array.clone()).add(&array2.into()).unwrap();

            result
                .with_array(|a| {
                    assert_ne!(a, &array); // ensure the addition makes a fully new copy
                    assert_eq!(a, &vec![LpcRef::from(123), LpcRef::from(4433)]);
                })
                .unwrap();
        }

        #[test]
        fn mapping_mapping() {
            let key1 = LpcRef::from(LpcString::from("key1"));
            let value1 = LpcRef::from(LpcString::from("value1"));
            let key2 = LpcRef::from(LpcString::from("key2"));
            let value2 = LpcRef::from(666);

            let mut hash1 = IndexMap::new();
            hash1.insert(key1.clone(), value1.clone());

            let mut hash2 = IndexMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = LpcRef::from(LpcMapping::new(hash1));
            let map2 = LpcRef::from(LpcMapping::new(hash2));

            let result = map.add(&map2);

            let mut expected = IndexMap::new();
            expected.insert(key1, value1);
            expected.insert(key2, value2);

            result
                .unwrap()
                .with_mapping(|m| {
                    assert_eq!(*m, expected);
                })
                .unwrap();
        }

        #[test]
        fn mapping_mapping_duplicate_keys() {
            let key1 = LpcRef::from(LpcString::from("key"));
            let value1 = LpcRef::from(LpcString::from("value1"));
            let key2 = LpcRef::from(LpcString::from("key"));
            let value2 = LpcRef::from(666);

            let mut hash1 = IndexMap::new();
            hash1.insert(key1, value1);

            let mut hash2 = IndexMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = LpcRef::from(LpcMapping::new(hash1));
            let map2 = LpcRef::from(LpcMapping::new(hash2));

            let result = map.add(&map2);

            let mut expected = IndexMap::new();
            expected.insert(key2, value2);

            result
                .unwrap()
                .with_mapping(|m| {
                    assert_eq!(*m, expected);
                })
                .unwrap();
        }

        #[test]
        fn add_mismatched() {
            let int = LpcRef::from(123);
            let array = LpcRef::from(LpcArray::new(vec![]));
            let result = int.add(&array);

            assert!(result.is_err());
        }
    }

    mod test_sub {
        use super::*;

        #[test]
        fn int_int_underflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt(LpcIntInner::MIN));
            let int2 = LpcRef::from(1);
            let result = int.sub(&int2);
            assert!(result.is_ok());
        }

        #[test]
        fn float_int() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.sub(&int);
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
            let result = float.sub(&int);
            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.sub(&float);
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
            let result = int.sub(&float);
            assert!(result.is_ok());
        }

        #[test]
        fn array_array() {
            let to_ref = |i| LpcRef::Int(LpcInt(i));
            let v1 = vec![1, 2, 3, 4, 5, 2, 4, 4, 4]
                .into_iter()
                .map(to_ref)
                .collect::<Vec<_>>();
            let v2 = vec![2, 4].into_iter().map(to_ref).collect::<Vec<_>>();
            let a1 = LpcRef::from(LpcArray::new(v1));
            let a2 = LpcRef::from(LpcArray::new(v2));

            let result = a1.sub(&a2);
            let expected = vec![1, 3, 5].into_iter().map(to_ref).collect::<Vec<_>>();

            result
                .unwrap()
                .with_array(|a| {
                    assert_eq!(*a, expected);
                })
                .unwrap();
        }
    }

    mod test_mul {
        use super::*;

        #[test]
        fn int_int_overflow_does_not_panic() {
            let int = LpcRef::Int(LpcInt::MAX);
            let int2 = LpcRef::from(2);
            let result = int.mul(&int2);
            assert!(result.is_ok());
        }

        #[test]
        fn string_int() {
            let string = LpcRef::from("foo");
            let int = LpcRef::from(4);
            let result = string.mul(&int);
            result
                .unwrap()
                .with_string(|s| assert_eq!(*s, "foofoofoofoo"))
                .unwrap();
        }

        #[test]
        fn int_string() {
            let string = LpcRef::from("foo");
            let int = LpcRef::from(4);
            let result = int.mul(&string);
            result
                .unwrap()
                .with_string(|x| {
                    assert_eq!(*x, "foofoofoofoo");
                })
                .unwrap();
        }

        #[test]
        fn string_int_overflow_does_not_panic() {
            let string = LpcRef::from(LpcString::from("1234567890abcdef"));
            let int = LpcRef::Int(LpcInt::MAX);
            let result = string.mul(&int);
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
            let result = float.mul(&int);
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
            let result = float.mul(&int);
            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.mul(&float);
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
            let result = int.mul(&float);
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
            let mut string = LpcRef::from(LpcString::from("foobar"));
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
            let mut string = LpcRef::from(LpcString::from("foobar"));
            assert_err!(string.dec());
        }
    }

    mod test_mark {
        use indexmap::IndexMap;

        use super::*;
        use crate::interpreter::lpc_mapping::LpcMapping;

        #[test]
        fn test_array() {
            let array = LpcArray::new(vec![LpcRef::from(1), LpcRef::from(2), LpcRef::from(3)]);
            let array_id = array.unique_id;
            let array = LpcRef::from(array);

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();

            array.mark(&mut marked, &mut processed).unwrap();

            assert!(processed.contains(*array_id.as_ref() as usize));
        }

        #[test]
        fn test_mapping() {
            let mapping = LpcMapping::new(IndexMap::new());
            let mapping_id = mapping.unique_id;
            let mapping = LpcRef::from(mapping);

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();

            mapping.mark(&mut marked, &mut processed).unwrap();

            assert!(processed.contains(*mapping_id.as_ref() as usize));
        }

        #[test]
        fn test_function() {
            let ptr = create!(FunctionPtr);
            let ptr_id = ptr.unique_id;
            let ptr = LpcRef::from(ptr);

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();

            ptr.mark(&mut marked, &mut processed).unwrap();

            assert!(processed.contains(*ptr_id.as_ref() as usize));
        }

        #[test]
        fn deadlock_regression_check() {
            let string = LpcRef::from("test");
            let string2 = string.clone();

            string
                .with_strings(&string2, |a, b| {
                    assert_eq!(a, b);
                })
                .expect("unreachable");

            let LpcRef::String(a) = string else {
                panic!("Expected string")
            };
            let LpcRef::String(b) = string2 else {
                panic!("Expected string")
            };
            assert!(Arc::ptr_eq(&a, &b));
        }
    }
}
