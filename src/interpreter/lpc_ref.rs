use std::{
    cmp::Ordering,
    fmt,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    ptr,
    sync::{Arc, Weak},
};

use bit_set::BitSet;
use decorum::Total;
use lpc_rs_core::{BaseFloat, LpcFloatInner, LpcIntInner, lpc_type::LpcType};
use lpc_rs_errors::{LpcError, Result, lpc_error};
use lpc_rs_utils::{string, string::concatenate_strings};
use tracing::{instrument, trace};

use crate::{
    compiler::ast::{binary_op_node::BinaryOperation, unary_op_node::UnaryOperation},
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        gc::mark::Mark,
        lpc_array::LpcArray,
        lpc_float::LpcFloat,
        lpc_int::LpcInt,
        lpc_mapping::LpcMapping,
        lpc_string::LpcString,
        process::Process,
        stm::{SVar, TxnHandle},
    },
};

pub const NULL: LpcRef = LpcRef::Int(LpcInt(0));

/// A transaction whose cells the unit tests mint into and read back from;
/// the payload helpers below need one to resolve array/mapping contents.
#[cfg(test)]
fn test_txn() -> TxnHandle {
    TxnHandle::empty()
}

/// Mint `array` into the test transaction and return its cell handle.
#[cfg(test)]
fn test_array_ref(txn: &TxnHandle, array: LpcArray) -> LpcRef {
    LpcRef::Array(txn.with(|t| t.mint_array(array)))
}

/// Mint `mapping` into the test transaction and return its cell handle.
#[cfg(test)]
fn test_mapping_ref(txn: &TxnHandle, mapping: LpcMapping) -> LpcRef {
    LpcRef::Mapping(txn.with(|t| t.mint_mapping(mapping)))
}

/// Represent a variable stored in a `Register`. Value types store the actual
/// value. Reference types store a reference to the actual value.
/// This type is intended to be cheap to clone.
#[derive(Clone)]
pub enum LpcRef {
    Float(LpcFloat),
    Int(LpcInt),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value. Strings are immutable once created, so no interior mutability.
    String(Arc<LpcString>),

    /// An array's identity in the transactional world. The contents live in
    /// the world under this handle's var, not here; the handle is a cheap
    /// copy of the `VarId`.
    Array(SVar<LpcArray>),

    /// A mapping's identity in the transactional world, as in [`LpcRef::Array`].
    Mapping(SVar<LpcMapping>),

    /// Reference type, pointing to an LPC `object`
    Object(Weak<Process>),

    /// Reference type, a function pointer or closure
    Function(Arc<FunctionPtr>),
}

/// A numeric operand pair after promotion: both ints, or both floats.
enum Numeric {
    Ints(LpcIntInner, LpcIntInner),
    Floats(LpcFloatInner, LpcFloatInner),
}

/// Zero to the tolerance the float divisor checks use.
fn is_zero(f: LpcFloatInner) -> bool {
    f.into_inner().abs() < BaseFloat::EPSILON
}

/// The shift count `y` names, modulo the word width; a negative count is
/// taken from the width.
fn shift_amount(y: LpcIntInner) -> u32 {
    let modulo = y % (LpcIntInner::BITS as LpcIntInner);
    if modulo < 0 {
        LpcIntInner::BITS - (modulo.unsigned_abs() as u32)
    } else {
        modulo as u32
    }
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

    /// The process behind an object ref, `None` once it is dropped;
    /// `live_object` also honours this attempt's destructs.
    pub fn as_object(&self) -> Option<Arc<Process>> {
        match self {
            LpcRef::Object(proc) => proc.upgrade(),
            _ => None,
        }
    }

    /// The object this ref points to, unless its process is dropped or this
    /// attempt destructed it.
    pub(crate) fn live_object(&self, txn: &TxnHandle) -> Option<Arc<Process>> {
        let process = self.as_object()?;
        process.is_live(txn).then_some(process)
    }

    /// Whether this is an object ref that no longer names a live object for `txn`.
    pub(crate) fn is_dead_object(&self, txn: &TxnHandle) -> bool {
        matches!(self, LpcRef::Object(_)) && self.live_object(txn).is_none()
    }

    /// This value as a mapping key: a destructed object is `0`.
    pub(crate) fn mapping_key(&self, txn: &TxnHandle) -> LpcRef {
        if self.is_dead_object(txn) {
            NULL
        } else {
            self.clone()
        }
    }

    /// The value as a condition sees it: `0`, `0.0` and a destructed object
    /// are false.
    pub(crate) fn is_truthy(&self, txn: &TxnHandle) -> bool {
        match self {
            LpcRef::Int(x) => x.0 != 0,
            LpcRef::Float(_) => self != &LpcRef::Float(Total::from(0.0).into()),
            LpcRef::Object(_) => self.live_object(txn).is_some(),
            _ => true,
        }
    }

    /// `==` as LPC sees it: a destructed object compares as `0`.
    pub(crate) fn eq_in(&self, other: &Self, txn: &TxnHandle) -> bool {
        match (self.is_dead_object(txn), other.is_dead_object(txn)) {
            (false, false) => self == other,
            (true, true) => true,
            (true, false) => other.is_null(),
            (false, true) => self.is_null(),
        }
    }

    /// The string payload, or `None` for a non-string.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            LpcRef::String(s) => Some(s.to_str()),
            _ => None,
        }
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
            LpcRef::Int(x) => {
                *x = x.wrapping_add(1).into();

                Ok(())
            }
            _ => Err(lpc_error!("runtime error: invalid increment")),
        }
    }

    pub fn dec(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(x) => {
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
            LpcRef::Object(x) if x.strong_count() == 0 => LpcType::Int(false),
            LpcRef::Object(_) => LpcType::Object(false),
            LpcRef::Function(_) => LpcType::Function(false),
        }
    }

    pub fn with_string<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(&LpcString) -> R,
    {
        match self {
            LpcRef::String(a) => Ok(f(a)),
            _ => Err(lpc_error!(
                "runtime error: invalid access. Expected string, actually {}",
                self.type_name()
            )),
        }
    }

    /// Resolve this array's contents from the transactional world and run `f`
    /// over them. The handle holds only the cell's var; the contents live in
    /// the world, so the transaction is required.
    pub(crate) fn with_array<F, R>(&self, txn: &TxnHandle, f: F) -> Result<R>
    where
        F: FnOnce(&LpcArray) -> R,
    {
        match self {
            LpcRef::Array(cell) => {
                let array = txn
                    .with(|t| t.read_array(cell.id))
                    .ok_or_else(|| self.expected_array_error())?;
                Ok(f(&array))
            }
            _ => Err(self.expected_array_error()),
        }
    }

    /// Resolve this mapping's contents from the transactional world and run
    /// `f` over them, as in [`with_array`]([`LpcRef::with_array`]).
    pub(crate) fn with_mapping<F, R>(&self, txn: &TxnHandle, f: F) -> Result<R>
    where
        F: FnOnce(&LpcMapping) -> R,
    {
        match self {
            LpcRef::Mapping(cell) => {
                let mapping = txn
                    .with(|t| t.read_mapping(cell.id))
                    .ok_or_else(|| self.expected_mapping_error())?;
                Ok(f(&mapping))
            }
            _ => Err(self.expected_mapping_error()),
        }
    }

    /// Copy-on-write this array cell: clone the world's contents into the
    /// attempt's changeset, run `f` on the clone, and write it back under the
    /// same var. The committed contents are never mutated in place.
    pub(crate) fn with_array_cow<F>(&self, txn: &TxnHandle, f: F) -> Result<()>
    where
        F: FnOnce(&mut LpcArray),
    {
        match self {
            LpcRef::Array(cell) => {
                txn.with(|t| t.with_array_cow(cell.id, f));
                Ok(())
            }
            _ => Err(self.expected_array_error()),
        }
    }

    /// Copy-on-write this mapping cell, as in [`with_array_cow`].
    pub(crate) fn with_mapping_cow<F>(&self, txn: &TxnHandle, f: F) -> Result<()>
    where
        F: FnOnce(&mut LpcMapping),
    {
        match self {
            LpcRef::Mapping(cell) => {
                txn.with(|t| t.with_mapping_cow(cell.id, f));
                Ok(())
            }
            _ => Err(self.expected_mapping_error()),
        }
    }

    fn expected_array_error(&self) -> Box<LpcError> {
        lpc_error!(
            "runtime error: invalid access. Expected array, actually {}",
            self.type_name()
        )
    }

    fn expected_mapping_error(&self) -> Box<LpcError> {
        lpc_error!(
            "runtime error: invalid access. Expected mapping, actually {}",
            self.type_name()
        )
    }

    /// The numeric pair `self`/`rhs` form, the int promoted when the kinds
    /// differ; `None` when either side is not a number.
    fn promote(&self, rhs: &Self) -> Option<Numeric> {
        match (self, rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Some(Numeric::Ints(x.0, y.0)),
            (LpcRef::Float(x), LpcRef::Float(y)) => Some(Numeric::Floats(x.0, y.0)),
            (LpcRef::Float(x), LpcRef::Int(y)) => {
                Some(Numeric::Floats(x.0, LpcFloatInner::from(y.0 as BaseFloat)))
            }
            (LpcRef::Int(x), LpcRef::Float(y)) => {
                Some(Numeric::Floats(LpcFloatInner::from(x.0 as BaseFloat), y.0))
            }
            _ => None,
        }
    }

    /// Apply an int-only operator, or fail with `op`'s mismatch error.
    fn int_binop<F>(&self, rhs: &Self, op: BinaryOperation, f: F) -> Result<Self>
    where
        F: FnOnce(LpcIntInner, LpcIntInner) -> LpcIntInner,
    {
        match (self, rhs) {
            (LpcRef::Int(x), LpcRef::Int(y)) => Ok(Self::Int(LpcInt(f(x.0, y.0)))),
            _ => Err(self.to_error(op, rhs)),
        }
    }

    pub(crate) fn add(&self, rhs: &Self, txn: &TxnHandle) -> Result<Self> {
        if let Some(numeric) = self.promote(rhs) {
            return Ok(match numeric {
                Numeric::Ints(x, y) => Self::from(x.wrapping_add(y)),
                Numeric::Floats(x, y) => Self::Float(LpcFloat(x + y)),
            });
        }

        match (self, rhs) {
            (LpcRef::Int(i), LpcRef::String(s)) => {
                Ok(LpcString::from(concatenate_strings(i.to_string(), s.to_str())?).into())
            }
            (LpcRef::String(a), LpcRef::String(b)) => {
                Ok(LpcString::from(concatenate_strings(a.to_string(), b.to_str())?).into())
            }
            (LpcRef::String(s), LpcRef::Int(i)) => {
                Ok(LpcString::from(concatenate_strings(s.to_string(), i.to_string())?).into())
            }
            (LpcRef::Array(cell), LpcRef::Array(rhs_cell)) => txn.with(|t| {
                let (a, b) = (t.read_array(cell.id), t.read_array(rhs_cell.id));
                let (a, b) = match (a, b) {
                    (Some(a), Some(b)) => (a, b),
                    _ => return Err(self.expected_array_error()),
                };
                Ok(LpcRef::Array(t.mint_array((*a).clone() + (*b).clone())))
            }),
            (LpcRef::Mapping(cell), LpcRef::Mapping(rhs_cell)) => txn.with(|t| {
                let (a, b) = (t.read_mapping(cell.id), t.read_mapping(rhs_cell.id));
                let (a, b) = match (a, b) {
                    (Some(a), Some(b)) => (a, b),
                    _ => return Err(self.expected_mapping_error()),
                };
                let mut new_map = (*a).clone();
                new_map.extend((*b).clone());
                Ok(LpcRef::Mapping(t.mint_mapping(new_map)))
            }),
            _ => Err(self.to_error(BinaryOperation::Add, rhs)),
        }
    }

    pub(crate) fn sub(&self, rhs: &Self, txn: &TxnHandle) -> Result<Self> {
        if let Some(numeric) = self.promote(rhs) {
            return Ok(match numeric {
                Numeric::Ints(x, y) => Self::from(x.wrapping_sub(y)),
                Numeric::Floats(x, y) => Self::Float(LpcFloat(x - y)),
            });
        }

        match (self, rhs) {
            (LpcRef::Array(cell), LpcRef::Array(rhs_cell)) => txn.with(|t| {
                let (a, b) = (t.read_array(cell.id), t.read_array(rhs_cell.id));
                let (a, b) = match (a, b) {
                    (Some(a), Some(b)) => (a, b),
                    _ => return Err(self.expected_array_error()),
                };
                let kept: Vec<LpcRef> =
                    (*a).iter().filter(|x| !(*b).contains(x)).cloned().collect();
                Ok(LpcRef::Array(t.mint_array(LpcArray::from_iter(kept))))
            }),
            _ => Err(self.to_error(BinaryOperation::Sub, rhs)),
        }
    }

    pub fn mul(&self, rhs: &Self) -> Result<Self> {
        if let Some(numeric) = self.promote(rhs) {
            return Ok(match numeric {
                Numeric::Ints(x, y) => Self::from(x.wrapping_mul(y)),
                Numeric::Floats(x, y) => Self::Float(LpcFloat(x * y)),
            });
        }

        match (self, rhs) {
            (LpcRef::String(s), LpcRef::Int(y)) => {
                Ok(LpcString::from(string::repeat_string(s.to_str(), y.0)?).into())
            }
            (LpcRef::Int(x), LpcRef::String(s)) => {
                Ok(LpcString::from(string::repeat_string(s.to_str(), x.0)?).into())
            }
            _ => Err(self.to_error(BinaryOperation::Mul, rhs)),
        }
    }

    pub fn div(&self, rhs: &Self) -> Result<Self> {
        let zero = || lpc_error!("runtime error: Division by zero");

        match self.promote(rhs) {
            Some(Numeric::Ints(_, 0)) => Err(zero()),
            Some(Numeric::Ints(x, y)) => Ok(Self::Int(LpcInt(x.wrapping_div(y)))),
            Some(Numeric::Floats(_, y)) if is_zero(y) => Err(zero()),
            Some(Numeric::Floats(x, y)) => Ok(Self::Float(LpcFloat(x / y))),
            None => Err(self.to_error(BinaryOperation::Div, rhs)),
        }
    }

    pub fn rem(&self, rhs: &Self) -> Result<Self> {
        let zero = || lpc_error!("runtime error: Remainder division by zero");

        match self.promote(rhs) {
            Some(Numeric::Ints(_, 0)) => Err(zero()),
            Some(Numeric::Ints(x, y)) => Ok(Self::Int(LpcInt(x.wrapping_rem(y)))),
            Some(Numeric::Floats(_, y)) if is_zero(y) => Err(zero()),
            Some(Numeric::Floats(x, y)) => Ok(Self::Float(LpcFloat(x % y))),
            None => Err(self.to_error(BinaryOperation::Mod, rhs)),
        }
    }

    pub fn bitand(&self, rhs: &Self) -> Result<Self> {
        self.int_binop(rhs, BinaryOperation::And, |x, y| x & y)
    }

    pub fn bitor(&self, rhs: &Self) -> Result<Self> {
        self.int_binop(rhs, BinaryOperation::Or, |x, y| x | y)
    }

    pub fn bitxor(&self, rhs: &Self) -> Result<Self> {
        self.int_binop(rhs, BinaryOperation::Xor, |x, y| x ^ y)
    }

    pub fn shl(&self, rhs: &Self) -> Result<Self> {
        self.int_binop(rhs, BinaryOperation::Shl, |x, y| {
            x.checked_shl(shift_amount(y)).unwrap_or(0)
        })
    }

    pub fn shr(&self, rhs: &Self) -> Result<Self> {
        self.int_binop(rhs, BinaryOperation::Shr, |x, y| {
            x.checked_shr(shift_amount(y)).unwrap_or(0)
        })
    }

    /// Logical not (the unary `!` operator): 1 for `0`, `0.0` and a
    /// destructed object, else 0.
    pub(crate) fn not(&self, txn: &TxnHandle) -> Self {
        match self {
            LpcRef::Int(x) => LpcRef::Int(LpcInt((*x == 0) as LpcIntInner)),
            LpcRef::Float(x) => LpcRef::Int(LpcInt((*x == 0.0) as LpcIntInner)),
            LpcRef::Object(_) => LpcRef::from(self.live_object(txn).is_none()),
            LpcRef::String(_) | LpcRef::Array(_) | LpcRef::Mapping(_) | LpcRef::Function(_) => NULL,
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
            LpcRef::Float(_)
            | LpcRef::Int(_)
            | LpcRef::String(_)
            | LpcRef::Object(_)
            | LpcRef::Array(_)
            | LpcRef::Mapping(_) => Ok(()),
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
        LpcRef::String(Arc::new(LpcString::from(value)))
    }
}

impl From<String> for LpcRef {
    #[inline]
    fn from(value: String) -> Self {
        LpcRef::String(Arc::new(LpcString::from(value)))
    }
}

impl From<LpcString> for LpcRef {
    #[inline]
    fn from(value: LpcString) -> Self {
        LpcRef::String(Arc::new(value))
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

/// A destructed object keeps its address hash, so a mapping keyed by it is
/// found by the ref and not by `0`.
impl Hash for LpcRef {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LpcRef::Float(x) => x.hash(state),
            LpcRef::Int(x) => x.hash(state),
            LpcRef::String(s) => s.hash(state),
            LpcRef::Array(x) => x.id.hash(state),
            LpcRef::Mapping(x) => x.id.hash(state),
            LpcRef::Object(x) => x.as_ptr().hash(state),
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
            (LpcRef::String(a), LpcRef::String(b)) => a == b,
            (LpcRef::Object(x), LpcRef::Object(y)) => Weak::ptr_eq(x, y),
            (LpcRef::Array(x), LpcRef::Array(y)) => x.id == y.id,
            (LpcRef::Mapping(x), LpcRef::Mapping(y)) => x.id == y.id,
            (LpcRef::Function(x), LpcRef::Function(y)) => ptr::eq(&**x, &**y),
            _ => false,
        }
    }
}

impl Eq for LpcRef {}

impl PartialEq<&Process> for LpcRef {
    fn eq(&self, other: &&Process) -> bool {
        match self {
            LpcRef::Object(x) => ptr::eq(x.as_ptr(), *other),
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
            (LpcRef::String(a), LpcRef::String(b)) => Some(a.cmp(b)),
            _ => None,
        }
    }
}

impl Display for LpcRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcRef::Float(x) => write!(f, "{x}"),
            LpcRef::Int(x) => write!(f, "{x}"),
            LpcRef::String(s) => write!(f, "{s}"),
            LpcRef::Array(x) => write!(f, "<array#{}>", x.id.as_u64()),
            LpcRef::Mapping(x) => write!(f, "<mapping#{}>", x.id.as_u64()),
            LpcRef::Object(x) => match x.upgrade() {
                Some(x) => write!(f, "{}", x),
                None => write!(f, "0"),
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
            LpcRef::String(s) => write!(f, "{s:?}"),
            LpcRef::Array(x) => write!(f, "Array({})", x.id.as_u64()),
            LpcRef::Mapping(x) => write!(f, "Mapping({})", x.id.as_u64()),
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

    mod test_object_identity {
        use std::hash::{BuildHasher, BuildHasherDefault, DefaultHasher};

        use super::*;
        use crate::interpreter::program::Program;

        fn hash_of(r: &LpcRef) -> u64 {
            BuildHasherDefault::<DefaultHasher>::default().hash_one(r)
        }

        #[test]
        fn handles_to_one_process_are_equal() {
            let process = Arc::new(Process::new(Program::default()));
            let a = LpcRef::from(Arc::downgrade(&process));
            let b = LpcRef::from(Arc::downgrade(&process));

            assert_eq!(a, b);
            assert_eq!(a, a.clone());
            assert_eq!(hash_of(&a), hash_of(&b));
            assert_eq!(a, &*process);
        }

        #[test]
        fn handles_to_different_processes_are_not_equal() {
            let p1 = Arc::new(Process::new(Program::default()));
            let p2 = Arc::new(Process::new(Program::default()));
            let a = LpcRef::from(Arc::downgrade(&p1));
            let b = LpcRef::from(Arc::downgrade(&p2));

            assert_ne!(a, b);
            assert_ne!(a, &*p2);
        }

        #[test]
        fn a_dropped_process_still_equals_itself() {
            let process = Arc::new(Process::new(Program::default()));
            let a = LpcRef::from(Arc::downgrade(&process));
            let b = a.clone();
            drop(process);

            assert_eq!(a, b);
            assert_eq!(hash_of(&a), hash_of(&b));
        }
    }

    mod test_liveness {
        use super::*;
        use crate::interpreter::{program::Program, stm::VarId};

        #[test]
        fn a_dropped_process_reads_as_zero() {
            let txn = TxnHandle::empty();
            let process = Arc::new(Process::new(Program::default()));
            let live = LpcRef::from(Arc::downgrade(&process));
            let other = LpcRef::from(Arc::downgrade(&Arc::new(Process::new(Program::default()))));
            assert!(live.is_truthy(&txn));
            assert!(!live.eq_in(&NULL, &txn));
            assert!(live.eq_in(&live.clone(), &txn));
            assert_eq!(live.not(&txn), LpcRef::from(0));
            assert_eq!(live.as_lpc_type(), LpcType::Object(false));

            drop(process);
            let dead = live;
            assert!(dead.live_object(&txn).is_none());
            assert!(!dead.is_truthy(&txn));
            assert!(dead.eq_in(&NULL, &txn));
            assert!(NULL.eq_in(&dead, &txn));
            assert!(dead.eq_in(&other, &txn));
            assert!(!dead.eq_in(&LpcRef::from(1), &txn));
            assert!(!dead.eq_in(&LpcRef::from(0.0), &txn));
            assert_eq!(dead.not(&txn), LpcRef::from(1));
            assert_eq!(dead.as_lpc_type(), LpcType::Int(false));
            assert_eq!(dead.to_string(), "0");
        }

        #[test]
        fn an_object_destructed_in_the_attempt_reads_as_zero_there_only() {
            let process = Arc::new(Process::new(Program::default()));
            let cell = VarId::new();
            process.cell.set(cell).unwrap();
            let lpc_ref = LpcRef::from(Arc::downgrade(&process));

            let destructing = TxnHandle::empty();
            destructing.with(|t| t.drop_var(cell));
            assert!(lpc_ref.live_object(&destructing).is_none());
            assert!(!lpc_ref.is_truthy(&destructing));
            assert!(lpc_ref.eq_in(&NULL, &destructing));
            assert_eq!(lpc_ref.not(&destructing), LpcRef::from(1));

            let another = TxnHandle::empty();
            assert!(lpc_ref.live_object(&another).is_some());
            assert!(lpc_ref.is_truthy(&another));
            assert!(!lpc_ref.eq_in(&NULL, &another));
        }
    }

    mod test_accessors {
        use super::*;
        use crate::interpreter::program::Program;

        #[test]
        fn as_object_upgrades_only_a_live_object() {
            let process = Arc::new(Process::new(Program::default()));
            let live = LpcRef::from(Arc::downgrade(&process));
            assert!(Arc::ptr_eq(&live.as_object().unwrap(), &process));

            drop(process);
            assert!(live.as_object().is_none());
            assert!(LpcRef::from(0).as_object().is_none());
            assert!(LpcRef::from("object").as_object().is_none());
        }

        #[test]
        fn as_str_binds_only_a_string() {
            assert_eq!(LpcRef::from("hi").as_str(), Some("hi"));
            assert_eq!(LpcRef::from(0).as_str(), None);
            assert_eq!(LpcRef::from(1.5).as_str(), None);
        }
    }

    mod test_add {
        use indexmap::IndexMap;

        use super::*;

        #[test]
        fn int_int() {
            let txn = test_txn();
            let int1 = LpcRef::from(123);
            let int2 = LpcRef::from(456);
            let result = int1.add(&int2, &txn);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, 579)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_int_overflow_wraps() {
            let txn = test_txn();
            let int1 = LpcRef::from(LpcIntInner::MAX);
            let int2 = LpcRef::from(1);
            let result = int1.add(&int2, &txn);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, LpcIntInner::MIN)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn string_string() {
            let txn = test_txn();
            let string1 = LpcRef::from("foo");
            let string2 = LpcRef::from(LpcString::from("bar"));
            let result = string1.add(&string2, &txn).unwrap();
            result.with_string(|x| assert_eq!(x, "foobar")).unwrap();
        }

        #[test]
        fn string_int() {
            let txn = test_txn();
            let string = LpcRef::from("foo");
            let int = LpcRef::from(123);
            let result = string.add(&int, &txn).unwrap();
            result.with_string(|x| assert_eq!(x, "foo123")).unwrap();
        }

        #[test]
        fn int_string() {
            let txn = test_txn();
            let string = LpcRef::from("foo");
            let int = LpcRef::from(123);
            let result = int.add(&string, &txn).unwrap();
            result
                .with_string(|x| {
                    assert_eq!(x, "123foo");
                })
                .unwrap();
        }

        #[test]
        fn float_int() {
            let txn = test_txn();
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.add(&int, &txn);
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_overflow_does_not_panic() {
            let txn = test_txn();
            let float = LpcRef::from(BaseFloat::MAX);
            let int = LpcRef::from(1);
            assert!((float.add(&int, &txn)).is_ok());
        }

        #[test]
        fn int_float() {
            let txn = test_txn();
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.add(&float, &txn);
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 789.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_overflow_does_not_panic() {
            let txn = test_txn();
            let int = LpcRef::Int(LpcInt(LpcIntInner::MAX));
            let float = LpcRef::from(1.0);
            assert!((int.add(&float, &txn)).is_ok());
        }

        #[test]
        fn array_array() {
            let txn = test_txn();
            let array = LpcArray::new(vec![LpcRef::from(123)]);
            let array2 = LpcArray::new(vec![LpcRef::from(4433)]);
            let result = test_array_ref(&txn, array)
                .add(&test_array_ref(&txn, array2), &txn)
                .unwrap();

            result
                .with_array(&txn, |a| {
                    assert_ne!(a, &vec![LpcRef::from(123)]); // the addition makes a fully new copy
                    assert_eq!(a, &vec![LpcRef::from(123), LpcRef::from(4433)]);
                })
                .unwrap();
        }

        #[test]
        fn mapping_mapping() {
            let txn = test_txn();
            let key1 = LpcRef::from(LpcString::from("key1"));
            let value1 = LpcRef::from(LpcString::from("value1"));
            let key2 = LpcRef::from(LpcString::from("key2"));
            let value2 = LpcRef::from(666);

            let mut hash1 = IndexMap::new();
            hash1.insert(key1.clone(), value1.clone());

            let mut hash2 = IndexMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = test_mapping_ref(&txn, LpcMapping::new(hash1));
            let map2 = test_mapping_ref(&txn, LpcMapping::new(hash2));

            let result = map.add(&map2, &txn);

            let mut expected = IndexMap::new();
            expected.insert(key1, value1);
            expected.insert(key2, value2);

            result
                .unwrap()
                .with_mapping(&txn, |m| {
                    assert_eq!(*m, expected);
                })
                .unwrap();
        }

        #[test]
        fn mapping_mapping_duplicate_keys() {
            let txn = test_txn();
            let key1 = LpcRef::from(LpcString::from("key"));
            let value1 = LpcRef::from(LpcString::from("value1"));
            let key2 = LpcRef::from(LpcString::from("key"));
            let value2 = LpcRef::from(666);

            let mut hash1 = IndexMap::new();
            hash1.insert(key1, value1);

            let mut hash2 = IndexMap::new();
            hash2.insert(key2.clone(), value2.clone());

            let map = test_mapping_ref(&txn, LpcMapping::new(hash1));
            let map2 = test_mapping_ref(&txn, LpcMapping::new(hash2));

            let result = map.add(&map2, &txn);

            let mut expected = IndexMap::new();
            expected.insert(key2, value2);

            result
                .unwrap()
                .with_mapping(&txn, |m| {
                    assert_eq!(*m, expected);
                })
                .unwrap();
        }

        #[test]
        fn add_mismatched() {
            let txn = test_txn();
            let int = LpcRef::from(123);
            let array = test_array_ref(&txn, LpcArray::new(vec![]));
            let result = int.add(&array, &txn);

            assert!(result.is_err());
        }
    }

    mod test_sub {
        use super::*;

        #[test]
        fn int_int_underflow_does_not_panic() {
            let txn = test_txn();
            let int = LpcRef::Int(LpcInt(LpcIntInner::MIN));
            let int2 = LpcRef::from(1);
            let result = int.sub(&int2, &txn);
            assert!(result.is_ok());
        }

        #[test]
        fn float_int() {
            let txn = test_txn();
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = float.sub(&int, &txn);
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, 543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn float_int_underflow_does_not_panic() {
            let txn = test_txn();
            let float = LpcRef::from(BaseFloat::MIN);
            let int = LpcRef::Int(LpcInt::MAX);
            let result = float.sub(&int, &txn);
            assert!(result.is_ok());
        }

        #[test]
        fn int_float() {
            let txn = test_txn();
            let float = LpcRef::from(666.66);
            let int = LpcRef::from(123);
            let result = int.sub(&float, &txn);
            if let Ok(LpcRef::Float(x)) = result {
                assert_eq!(x, -543.66)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn int_float_underflow_does_not_panic() {
            let txn = test_txn();
            let int = LpcRef::Int(LpcInt::MIN);
            let float = LpcRef::from(1.0);
            let result = int.sub(&float, &txn);
            assert!(result.is_ok());
        }

        #[test]
        fn array_array() {
            let txn = test_txn();
            let to_ref = |i| LpcRef::Int(LpcInt(i));
            let v1 = vec![1, 2, 3, 4, 5, 2, 4, 4, 4]
                .into_iter()
                .map(to_ref)
                .collect::<Vec<_>>();
            let v2 = vec![2, 4].into_iter().map(to_ref).collect::<Vec<_>>();
            let a1 = test_array_ref(&txn, LpcArray::new(v1));
            let a2 = test_array_ref(&txn, LpcArray::new(v2));

            let result = a1.sub(&a2, &txn);
            let expected = vec![1, 3, 5].into_iter().map(to_ref).collect::<Vec<_>>();

            result
                .unwrap()
                .with_array(&txn, |a| {
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

    mod test_zero_divisor_messages {
        use super::*;

        #[test]
        fn message_follows_the_operator() {
            let int = LpcRef::from(1);
            let float = LpcRef::from(1.0);
            let zero = LpcRef::from(0);
            let fzero = LpcRef::from(0.0);
            let division = "runtime error: Division by zero";
            let remainder = "runtime error: Remainder division by zero";
            let cases = [
                ("int / int", int.div(&zero), division),
                ("float / float", float.div(&fzero), division),
                ("float / int", float.div(&zero), division),
                ("int / float", int.div(&fzero), division),
                ("int % int", int.rem(&zero), remainder),
                ("float % float", float.rem(&fzero), remainder),
                ("float % int", float.rem(&zero), remainder),
                ("int % float", int.rem(&fzero), remainder),
            ];

            for (label, result, message) in cases {
                assert_eq!(result.unwrap_err().to_string(), message, "{label}");
            }
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

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();

            array.mark(&mut marked, &mut processed).unwrap();

            assert!(processed.contains(*array_id.as_ref() as usize));
        }

        #[test]
        fn test_mapping() {
            let mapping = LpcMapping::new(IndexMap::new());
            let mapping_id = mapping.unique_id;

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
    }
}
