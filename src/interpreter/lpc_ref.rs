use lpc_rs_core::null_mode::NullMode;
use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    ptr,
    sync::{Arc, Weak},
};

use bytes::Bytes;
use decorum::Total;
use lpc_rs_asm::instruction::Comparison;
use lpc_rs_core::{BaseFloat, LpcFloatInner, LpcIntInner, lpc_type::LpcType};
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::constant::LpcConstant;
use lpc_rs_utils::lpc_bytes;
use lpc_rs_utils::lpc_string::LpcString;
use lpc_rs_utils::string;

use crate::{
    compiler::ast::{binary_op_node::BinaryOperation, unary_op_node::UnaryOperation},
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        lpc_array::LpcArray,
        lpc_class::LpcClass,
        lpc_float::LpcFloat,
        lpc_int::LpcInt,
        lpc_mapping::LpcMapping,
        process::Process,
        stm::{SVar, Transaction, TxnHandle},
    },
};

pub const NULL: LpcRef = LpcRef::Int(LpcInt(0));

/// The refusal every operator answers for a `bytes` beside a `string`.
pub(crate) const BYTES_STRING_MIX: &str =
    "mismatched types: a bytes and a string do not mix; convert with to_text() or to_bytes()";

/// A transaction whose cells the unit tests mint into and read back from;
/// the payload helpers below need one to resolve array/mapping contents.
#[cfg(test)]
fn test_txn() -> TxnHandle {
    TxnHandle::empty()
}

/// A `bytes` value for the tests; no LPC source can write one.
#[cfg(test)]
pub(crate) fn test_bytes(v: &[u8]) -> LpcRef {
    LpcRef::from(Bytes::copy_from_slice(v))
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
    Nil,
    Float(LpcFloat),
    Int(LpcInt),

    /// Reference type, and stores a reference-counting pointer to the actual
    /// value. Strings are immutable once created, so no interior mutability.
    String(Arc<LpcString>),

    /// An immutable byte sequence, behind an `Arc` so the register stays 16
    /// bytes.
    Bytes(Arc<Bytes>),

    /// An array's identity in the transactional world. The contents live in
    /// the world under this handle's var, not here; the handle is a cheap
    /// copy of the `VarId`.
    Array(SVar<LpcArray>),

    /// A mapping's identity in the transactional world, as in [`LpcRef::Array`].
    Mapping(SVar<LpcMapping>),

    /// A class instance in the transactional world.
    Class(SVar<LpcClass>),

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

/// Compare without rounding the integer, retaining the float ordering of NaN last.
fn cmp_int_float(integer: LpcIntInner, float: LpcFloatInner) -> Ordering {
    let value = float.into_inner();
    // The exclusive upper bound is exact; casting i64::MAX to f64 rounds up to it.
    if value >= -(LpcIntInner::MIN as BaseFloat) || value.is_nan() {
        return Ordering::Less;
    }
    if value < LpcIntInner::MIN as BaseFloat {
        return Ordering::Greater;
    }
    integer
        .cmp(&(value as LpcIntInner))
        .then_with(|| LpcFloatInner::from(0.0).cmp(&LpcFloatInner::from(value.fract())))
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

/// `x / y` on ints; `None` for a zero divisor.
#[inline]
pub(crate) fn int_div(x: LpcIntInner, y: LpcIntInner) -> Option<LpcIntInner> {
    (y != 0).then(|| x.wrapping_div(y))
}

/// `x % y` on ints; `None` for a zero divisor.
#[inline]
pub(crate) fn int_rem(x: LpcIntInner, y: LpcIntInner) -> Option<LpcIntInner> {
    (y != 0).then(|| x.wrapping_rem(y))
}

/// `x << y` on ints, the count as [`shift_amount`] reads it.
#[inline]
pub(crate) fn int_shl(x: LpcIntInner, y: LpcIntInner) -> LpcIntInner {
    x.checked_shl(shift_amount(y)).unwrap_or(0)
}

/// `x >> y` on ints, the count as [`shift_amount`] reads it.
#[inline]
pub(crate) fn int_shr(x: LpcIntInner, y: LpcIntInner) -> LpcIntInner {
    x.checked_shr(shift_amount(y)).unwrap_or(0)
}

/// The error for a collection ref whose contents are not in the reading
/// transaction's world.
fn gone_error(kind: &str) -> LpcError {
    LpcError::runtime(format!(
        "invalid access. The {kind}'s contents are not in this transaction's world"
    ))
}

/// Comparison keys borrow the members unless destroyed references need the configured null value.
fn array_set_keys<'a>(
    members: &'a [LpcRef],
    transaction: &mut Transaction,
    mode: NullMode,
) -> Cow<'a, [LpcRef]> {
    let mut keys = Cow::Borrowed(members);
    for (index, member) in members.iter().enumerate() {
        if let LpcRef::Object(weak) = member
            && weak.upgrade().is_none_or(|p| !p.is_live_in(transaction))
        {
            keys.to_mut()[index] = LpcRef::nil(mode);
        }
    }
    keys
}

impl LpcRef {
    /// The configured absence value.
    pub fn nil(mode: NullMode) -> Self {
        match mode {
            NullMode::Zero => NULL,
            NullMode::Distinct => Self::Nil,
        }
    }

    /// An uninitialized declaration's value in this mode.
    pub fn default_for(type_: LpcType, mode: NullMode) -> Self {
        if mode == NullMode::Zero {
            return NULL;
        }
        match type_ {
            LpcType::Int(false) => 0.into(),
            LpcType::Float(false) => 0.0.into(),
            _ => Self::Nil,
        }
    }

    pub(crate) fn is_null_in(&self, txn: &TxnHandle) -> bool {
        self == &txn.nil() || self.is_dead_object(txn)
    }

    /// Get the type name of the underlying data of this var.
    pub fn type_name(&self) -> &str {
        match self {
            LpcRef::Nil => "nil",
            LpcRef::Float(_) => "float",
            LpcRef::Int(_) => "int",
            LpcRef::String(_) => "string",
            LpcRef::Bytes(_) => "bytes",
            LpcRef::Array(_) => "array",
            LpcRef::Mapping(_) => "mapping",
            LpcRef::Class(_) => "class",
            LpcRef::Object(_) => "object",
            LpcRef::Function(_) => "function",
        }
    }

    /// Whether this is either null representation, without selecting a VM mode.
    pub fn is_null(&self) -> bool {
        matches!(self, LpcRef::Nil | LpcRef::Int(LpcInt(0)))
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

    /// This value as a mapping key: a destructed object uses the selected null value.
    pub(crate) fn mapping_key(&self, txn: &TxnHandle) -> LpcRef {
        if self.is_dead_object(txn) {
            txn.nil()
        } else {
            self.clone()
        }
    }

    /// The value as a condition sees it: `nil`, `0`, `0.0` and destructed objects are false.
    pub(crate) fn is_truthy(&self, txn: &TxnHandle) -> bool {
        match self {
            LpcRef::Nil => false,
            LpcRef::Int(x) => x.0 != 0,
            LpcRef::Float(_) => self != &LpcRef::Float(Total::from(0.0).into()),
            LpcRef::Object(_) => self.live_object(txn).is_some(),
            _ => true,
        }
    }

    /// `==` as LPC sees it: a destructed object compares as the selected null value.
    pub(crate) fn eq_in(&self, other: &Self, txn: &TxnHandle) -> bool {
        match (self.is_dead_object(txn), other.is_dead_object(txn)) {
            (false, false) => self.value_eq(other),
            (true, true) => true,
            (true, false) => other.value_eq(&txn.nil()),
            (false, true) => self.value_eq(&txn.nil()),
        }
    }

    /// Value equality includes exact numeric equality across int and float types.
    fn value_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(x), Self::Float(y)) | (Self::Float(y), Self::Int(x)) => {
                cmp_int_float(x.0, y.0).is_eq()
            }
            _ => self == other,
        }
    }

    /// Whether `self kind other` holds; nonnumeric pairs of different types are unordered.
    pub(crate) fn compare(&self, kind: Comparison, other: &Self, txn: &TxnHandle) -> bool {
        match kind {
            Comparison::Eq => self.eq_in(other, txn),
            Comparison::Ne => !self.eq_in(other, txn),
            ordering => self
                .natural_cmp(other)
                .is_some_and(|result| ordering.holds(result, Ordering::Equal)),
        }
    }

    /// Natural order: ints and floats by value, together; strings by
    /// content; `None` for any other pair.
    pub(crate) fn natural_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LpcRef::Int(x), LpcRef::Float(y)) => Some(cmp_int_float(x.0, y.0)),
            (LpcRef::Float(x), LpcRef::Int(y)) => Some(cmp_int_float(y.0, x.0).reverse()),
            _ => self.partial_cmp(other),
        }
    }

    /// The string payload, or `None` for a non-string.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            LpcRef::String(s) => Some(s.to_str()),
            _ => None,
        }
    }

    /// The byte payload, or `None` for anything but a `bytes`.
    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            LpcRef::Bytes(b) => Some(b),
            _ => None,
        }
    }

    fn to_error(&self, op: BinaryOperation, right: &LpcRef) -> LpcError {
        LpcError::runtime(format!(
            "mismatched types: {} ({}) {} {} ({})",
            self,
            self.type_name(),
            op,
            right,
            right.type_name()
        ))
    }

    fn to_unary_op_error(&self, op: UnaryOperation) -> LpcError {
        LpcError::runtime(format!(
            "mismatched types: {} {} ({})",
            op,
            self,
            self.type_name()
        ))
    }

    #[inline]
    pub fn inc(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(x) => {
                *x = x.wrapping_add(1).into();

                Ok(())
            }
            _ => Err(LpcError::runtime("invalid increment")),
        }
    }

    #[inline]
    pub fn dec(&mut self) -> Result<()> {
        match self {
            LpcRef::Int(x) => {
                *x = x.wrapping_sub(1).into();
                Ok(())
            }
            _ => Err(LpcError::runtime("invalid decrement")),
        }
    }

    pub(crate) fn as_lpc_type(&self, txn: &TxnHandle) -> LpcType {
        match self {
            LpcRef::Nil => LpcType::Nil,
            LpcRef::Float(_) => LpcType::Float(false),
            LpcRef::Int(_) => LpcType::Int(false),
            LpcRef::String(_) => LpcType::String(false),
            LpcRef::Bytes(_) => LpcType::Bytes(false),
            LpcRef::Array(_) => LpcType::Mixed(true), // this could be better
            LpcRef::Mapping(_) => LpcType::Mapping(false),
            LpcRef::Class(cell) => txn
                .with(|t| t.read_class(cell.id))
                .map_or(LpcType::Void, |instance| LpcType::Class(instance.id, false)),
            LpcRef::Object(_) if self.is_dead_object(txn) => txn.nil().as_lpc_type(txn),
            LpcRef::Object(_) => LpcType::Object(false),
            LpcRef::Function(_) => LpcType::Function(false),
        }
    }

    /// Whether this value fits `target` under the selected null rules; arrays are shallow.
    pub(crate) fn passes_cast(&self, target: LpcType, txn: &TxnHandle) -> bool {
        if txn.null_mode() == NullMode::Zero && self.is_null_in(txn) {
            return true;
        }
        target.matches_type(self.as_lpc_type(txn))
    }

    pub fn with_string<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(&LpcString) -> R,
    {
        match self {
            LpcRef::String(a) => Ok(f(a)),
            _ => Err(LpcError::runtime(format!(
                "invalid access. Expected string, actually {}",
                self.type_name()
            ))),
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
                    .ok_or_else(|| gone_error("array"))?;
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
                    .ok_or_else(|| gone_error("mapping"))?;
                Ok(f(&mapping))
            }
            _ => Err(self.expected_mapping_error()),
        }
    }

    /// Copy-on-write this array cell: clone the world's contents into the
    /// attempt's changeset, run `f` on the clone, and write it back under the
    /// same var. The committed contents are never mutated in place.
    #[cfg(test)]
    pub(crate) fn with_array_cow<F>(&self, txn: &TxnHandle, f: F) -> Result<()>
    where
        F: FnOnce(&mut LpcArray) -> Result<()>,
    {
        match self {
            LpcRef::Array(cell) => txn.with(|t| t.with_array_cow(cell.id, f)),
            _ => Err(self.expected_array_error()),
        }
    }

    /// Drop this mapping's keys that name destructed objects, as `sizeof`,
    /// `keys` and `values` answer without them.
    pub(crate) fn drop_dead_keys(&self, txn: &TxnHandle) -> Result<()> {
        // The COW closure holds the transaction lock, so liveness is checked before it.
        let dead = self.with_mapping(txn, |m| {
            m.keys()
                .filter(|k| k.is_dead_object(txn))
                .cloned()
                .collect::<Vec<_>>()
        })?;
        if !dead.is_empty() {
            self.with_mapping_cow(txn, |m| {
                m.retain(|k, _| !dead.contains(k));
                Ok(())
            })?;
        }
        Ok(())
    }

    /// Copy-on-write this mapping into the attempt's changeset, leaving the committed contents unchanged.
    pub(crate) fn with_mapping_cow<F>(&self, txn: &TxnHandle, f: F) -> Result<()>
    where
        F: FnOnce(&mut LpcMapping) -> Result<()>,
    {
        match self {
            LpcRef::Mapping(cell) => txn.with(|t| t.with_mapping_cow(cell.id, f)),
            _ => Err(self.expected_mapping_error()),
        }
    }

    fn expected_array_error(&self) -> LpcError {
        LpcError::runtime(format!(
            "invalid access. Expected array, actually {}",
            self.type_name()
        ))
    }

    fn expected_mapping_error(&self) -> LpcError {
        LpcError::runtime(format!(
            "invalid access. Expected mapping, actually {}",
            self.type_name()
        ))
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
                Ok(LpcString::concat(&i.to_string(), s.to_str())?.into())
            }
            (LpcRef::String(a), LpcRef::String(b)) => {
                Ok(LpcString::concat(a.to_str(), b.to_str())?.into())
            }
            (LpcRef::String(s), LpcRef::Int(i)) => {
                Ok(LpcString::concat(s.to_str(), &i.to_string())?.into())
            }
            (LpcRef::Bytes(a), LpcRef::Bytes(b)) => Ok(LpcRef::from(lpc_bytes::concat(a, b)?)),
            (LpcRef::Bytes(_), LpcRef::String(_)) | (LpcRef::String(_), LpcRef::Bytes(_)) => {
                Err(LpcError::runtime(BYTES_STRING_MIX))
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
                let a_keys = array_set_keys(&a, t, txn.null_mode());
                let b_keys = array_set_keys(&b, t, txn.null_mode());
                let kept: Vec<LpcRef> = a
                    .iter()
                    .zip(a_keys.iter())
                    .filter(|(_, key)| !b_keys.contains(key))
                    .map(|(member, _)| member.clone())
                    .collect();
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
        let zero = || LpcError::runtime("Division by zero");

        match self.promote(rhs) {
            Some(Numeric::Ints(x, y)) => int_div(x, y).map(Self::from).ok_or_else(zero),
            Some(Numeric::Floats(_, y)) if is_zero(y) => Err(zero()),
            Some(Numeric::Floats(x, y)) => Ok(Self::Float(LpcFloat(x / y))),
            None => Err(self.to_error(BinaryOperation::Div, rhs)),
        }
    }

    pub fn rem(&self, rhs: &Self) -> Result<Self> {
        let zero = || LpcError::runtime("Remainder division by zero");

        match self.promote(rhs) {
            Some(Numeric::Ints(x, y)) => int_rem(x, y).map(Self::from).ok_or_else(zero),
            Some(Numeric::Floats(_, y)) if is_zero(y) => Err(zero()),
            Some(Numeric::Floats(x, y)) => Ok(Self::Float(LpcFloat(x % y))),
            None => Err(self.to_error(BinaryOperation::Mod, rhs)),
        }
    }

    pub(crate) fn bitand(&self, rhs: &Self, txn: &TxnHandle) -> Result<Self> {
        match (self, rhs) {
            (LpcRef::Array(cell), LpcRef::Array(rhs_cell)) => txn.with(|t| {
                let (a, b) = (t.read_array(cell.id), t.read_array(rhs_cell.id));
                let (a, b) = match (a, b) {
                    (Some(a), Some(b)) => (a, b),
                    _ => return Err(self.expected_array_error()),
                };
                let a_keys = array_set_keys(&a, t, txn.null_mode());
                let b_keys = array_set_keys(&b, t, txn.null_mode());
                let kept: Vec<LpcRef> = a
                    .iter()
                    .zip(a_keys.iter())
                    .filter(|(_, key)| b_keys.contains(key))
                    .map(|(member, _)| member.clone())
                    .collect();
                Ok(LpcRef::Array(t.mint_array(LpcArray::from_iter(kept))))
            }),
            _ => self.int_binop(rhs, BinaryOperation::And, |x, y| x & y),
        }
    }

    /// `|`: ints bitwise; arrays as the union every reference driver
    /// answers, the left kept as it is and the right's members absent from
    /// the left appended in the right's order, its duplicates included.
    pub(crate) fn bitor(&self, rhs: &Self, txn: &TxnHandle) -> Result<Self> {
        match (self, rhs) {
            (LpcRef::Array(cell), LpcRef::Array(rhs_cell)) => txn.with(|t| {
                let (Some(a), Some(b)) = (t.read_array(cell.id), t.read_array(rhs_cell.id)) else {
                    return Err(self.expected_array_error());
                };
                let a_keys = array_set_keys(&a, t, txn.null_mode());
                let b_keys = array_set_keys(&b, t, txn.null_mode());
                let mut joined: Vec<LpcRef> = (*a).iter().cloned().collect();
                joined.extend(
                    b.iter()
                        .zip(b_keys.iter())
                        .filter(|(_, key)| !a_keys.contains(key))
                        .map(|(member, _)| member.clone()),
                );
                Ok(LpcRef::Array(t.mint_array(LpcArray::from_iter(joined))))
            }),
            _ => self.int_binop(rhs, BinaryOperation::Or, |x, y| x | y),
        }
    }

    pub fn bitxor(&self, rhs: &Self) -> Result<Self> {
        self.int_binop(rhs, BinaryOperation::Xor, |x, y| x ^ y)
    }

    pub fn shl(&self, rhs: &Self) -> Result<Self> {
        self.int_binop(rhs, BinaryOperation::Shl, int_shl)
    }

    pub fn shr(&self, rhs: &Self) -> Result<Self> {
        self.int_binop(rhs, BinaryOperation::Shr, int_shr)
    }

    /// Logical not (the unary `!` operator): 1 for `nil`, `0`, `0.0` and a
    /// destructed object, else 0.
    pub(crate) fn not(&self, txn: &TxnHandle) -> Self {
        Self::from(!self.is_truthy(txn))
    }

    /// Impl _bitwise_ Not for ints, (i.e. the unary `~` operator)
    pub fn bitnot(&self) -> Result<Self> {
        match &self {
            LpcRef::Int(x) => Ok(Self::Int(LpcInt(!x.0))),
            _ => Err(self.to_unary_op_error(UnaryOperation::BitwiseNot)),
        }
    }

    /// `-self`; the minimum int wraps to itself.
    pub fn negate(&self) -> Result<Self> {
        match self {
            LpcRef::Int(x) => Ok(Self::Int(LpcInt(x.0.wrapping_neg()))),
            LpcRef::Float(f) => Ok(Self::from(-f.0.into_inner())),
            _ => Err(self.to_unary_op_error(UnaryOperation::Negate)),
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

impl From<&LpcConstant> for LpcRef {
    #[inline]
    fn from(constant: &LpcConstant) -> Self {
        match constant {
            LpcConstant::Nil => LpcRef::Nil,
            LpcConstant::Int(x) => LpcRef::Int(LpcInt(*x)),
            LpcConstant::Float(x) => LpcRef::Float(LpcFloat(*x)),
            LpcConstant::String(x) => LpcRef::String(x.clone()),
        }
    }
}

impl From<LpcString> for LpcRef {
    #[inline]
    fn from(value: LpcString) -> Self {
        LpcRef::String(Arc::new(value))
    }
}

impl From<Bytes> for LpcRef {
    #[inline]
    fn from(value: Bytes) -> Self {
        LpcRef::Bytes(Arc::new(value))
    }
}

impl From<Vec<u8>> for LpcRef {
    #[inline]
    fn from(value: Vec<u8>) -> Self {
        LpcRef::from(Bytes::from(value))
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
            LpcRef::Nil => std::mem::discriminant(self).hash(state),
            LpcRef::Float(x) => x.hash(state),
            LpcRef::Int(x) => x.hash(state),
            LpcRef::String(s) => s.hash(state),
            LpcRef::Bytes(b) => b.hash(state),
            LpcRef::Array(x) => x.id.hash(state),
            LpcRef::Mapping(x) => x.id.hash(state),
            LpcRef::Class(x) => x.id.hash(state),
            LpcRef::Object(x) => x.as_ptr().hash(state),
            LpcRef::Function(x) => ptr::hash(&**x, state),
        }
    }
}

impl PartialEq for LpcRef {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LpcRef::Nil, LpcRef::Nil) => true,
            (LpcRef::Float(x), LpcRef::Float(y)) => x == y,
            (LpcRef::Int(x), LpcRef::Int(y)) => x == y,
            (LpcRef::String(a), LpcRef::String(b)) => a == b,
            (LpcRef::Bytes(a), LpcRef::Bytes(b)) => a == b,
            (LpcRef::Object(x), LpcRef::Object(y)) => Weak::ptr_eq(x, y),
            (LpcRef::Array(x), LpcRef::Array(y)) => x.id == y.id,
            (LpcRef::Mapping(x), LpcRef::Mapping(y)) => x.id == y.id,
            (LpcRef::Class(x), LpcRef::Class(y)) => x.id == y.id,
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
            (LpcRef::Bytes(a), LpcRef::Bytes(b)) => Some(a.cmp(b)),
            _ => None,
        }
    }
}

impl Display for LpcRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcRef::Nil => f.write_str("nil"),
            LpcRef::Float(x) => write!(f, "{x}"),
            LpcRef::Int(x) => write!(f, "{x}"),
            LpcRef::String(s) => write!(f, "{s}"),
            LpcRef::Bytes(b) => write!(f, "{}", lpc_bytes::literal(b)),
            LpcRef::Array(x) => write!(f, "<array#{}>", x.id.as_u64()),
            LpcRef::Mapping(x) => write!(f, "<mapping#{}>", x.id.as_u64()),
            LpcRef::Class(x) => write!(f, "<class#{}>", x.id.as_u64()),
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
            LpcRef::Nil => f.write_str("nil"),
            LpcRef::Float(x) => write!(f, "{x:?}"),
            LpcRef::Int(x) => write!(f, "{x:?}"),
            LpcRef::String(s) => write!(f, "{s:?}"),
            LpcRef::Bytes(b) => write!(f, "{}", lpc_bytes::literal(b)),
            LpcRef::Array(x) => write!(f, "Array({})", x.id.as_u64()),
            LpcRef::Mapping(x) => write!(f, "Mapping({})", x.id.as_u64()),
            LpcRef::Class(x) => write!(f, "Class({})", x.id.as_u64()),
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

    /// A register holds an [`LpcRef`] and is copied on every move, so a new
    /// variant's payload belongs behind a pointer.
    #[test]
    fn lpc_ref_stays_sixteen_bytes() {
        assert_eq!(std::mem::size_of::<super::LpcRef>(), 16);
    }

    mod compare {
        use lpc_rs_asm::instruction::Comparison;

        use super::super::*;

        fn under(kind: Comparison, a: &LpcRef, b: &LpcRef) -> bool {
            a.compare(kind, b, &TxnHandle::empty())
        }

        #[test]
        fn a_mismatched_pair_is_false_under_every_ordering() {
            let s = LpcRef::from("a");
            let i = LpcRef::from(1);
            let orderings = [
                Comparison::Lt,
                Comparison::Lte,
                Comparison::Gt,
                Comparison::Gte,
            ];
            assert!(
                orderings
                    .iter()
                    .all(|&kind| !under(kind, &s, &i) && !under(kind, &i, &s))
            );
        }

        #[test]
        fn a_mismatched_pair_is_unequal() {
            let s = LpcRef::from("a");
            let i = LpcRef::from(1);
            assert_eq!(
                (under(Comparison::Eq, &s, &i), under(Comparison::Ne, &s, &i)),
                (false, true)
            );
        }

        #[test]
        fn ints_order() {
            let one = LpcRef::from(1);
            let two = LpcRef::from(2);
            let answers: Vec<bool> = Comparison::ALL
                .iter()
                .map(|&k| under(k, &one, &two))
                .collect();
            assert_eq!(answers, [true, true, false, false, false, true]);
        }

        #[test]
        fn equal_ints_hold_under_lte_gte_and_eq() {
            let two = LpcRef::from(2);
            let answers: Vec<bool> = Comparison::ALL
                .iter()
                .map(|&k| under(k, &two, &two))
                .collect();
            assert_eq!(answers, [false, true, false, true, true, false]);
        }

        #[test]
        fn mixed_numeric_comparisons_are_exact_at_float_and_integer_boundaries() {
            use Ordering::{Equal, Greater, Less};

            let cases = [
                (1, 1.0, Equal),
                (1, 1.5, Less),
                (2, 1.5, Greater),
                (-1, -1.5, Greater),
                (-2, -1.5, Less),
                (0, 0.0, Equal),
                (0, -0.0, Equal),
                (0, BaseFloat::from_bits(1), Less),
                (0, -BaseFloat::from_bits(1), Greater),
                (9_007_199_254_740_991, 9_007_199_254_740_992.0, Less),
                (9_007_199_254_740_992, 9_007_199_254_740_992.0, Equal),
                (9_007_199_254_740_993, 9_007_199_254_740_992.0, Greater),
                (-9_007_199_254_740_993, -9_007_199_254_740_992.0, Less),
                (LpcIntInner::MAX, 9_223_372_036_854_775_808.0, Less),
                (LpcIntInner::MAX, 9_223_372_036_854_774_784.0, Greater),
                (LpcIntInner::MIN, -9_223_372_036_854_775_808.0, Equal),
                (LpcIntInner::MIN + 1, -9_223_372_036_854_775_808.0, Greater),
                (LpcIntInner::MIN, -9_223_372_036_854_774_784.0, Less),
                (LpcIntInner::MIN, -9_223_372_036_854_777_856.0, Greater),
                (LpcIntInner::MAX, BaseFloat::MAX, Less),
                (LpcIntInner::MIN, BaseFloat::MIN, Greater),
                (LpcIntInner::MAX, BaseFloat::INFINITY, Less),
                (LpcIntInner::MIN, BaseFloat::NEG_INFINITY, Greater),
                (0, BaseFloat::NAN, Less),
            ];
            for (integer, float, expected) in cases {
                let i = LpcRef::from(integer);
                let f = LpcRef::from(float);
                for (left, right, order) in [(&i, &f, expected), (&f, &i, expected.reverse())] {
                    assert_eq!(left.natural_cmp(right), Some(order), "{left}, {right}");
                    for kind in Comparison::ALL {
                        assert_eq!(
                            under(kind, left, right),
                            kind.holds(order, Equal),
                            "{left} {kind} {right}"
                        );
                    }
                }
            }
        }
    }

    use super::*;
    use crate::interpreter::lpc_array::LpcArray;

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
            assert_eq!(
                live.as_lpc_type(&TxnHandle::empty()),
                LpcType::Object(false)
            );

            drop(process);
            let dead = live;
            assert!(dead.live_object(&txn).is_none());
            assert!(!dead.is_truthy(&txn));
            assert!(dead.eq_in(&NULL, &txn));
            assert!(NULL.eq_in(&dead, &txn));
            assert!(dead.eq_in(&other, &txn));
            assert!(!dead.eq_in(&LpcRef::from(1), &txn));
            assert!(dead.eq_in(&LpcRef::from(0.0), &txn));
            assert_eq!(dead.not(&txn), LpcRef::from(1));
            assert_eq!(dead.as_lpc_type(&TxnHandle::empty()), LpcType::Int(false));
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
            another.with(|t| t.write_process(cell, process.clone()));
            assert!(lpc_ref.live_object(&another).is_some());
            assert!(lpc_ref.is_truthy(&another));
            assert!(!lpc_ref.eq_in(&NULL, &another));
        }
    }

    mod test_passes_cast {
        use super::*;
        use crate::interpreter::{program::Program, stm::VarId};

        #[test]
        fn zero_passes_every_cast() {
            let txn = TxnHandle::empty();
            let zero = LpcRef::from(0);
            assert!(zero.passes_cast(LpcType::String(false), &txn));
            assert!(zero.passes_cast(LpcType::Object(true), &txn));
        }

        #[test]
        fn a_value_passes_only_its_own_type_or_mixed() {
            let txn = TxnHandle::empty();
            let five = LpcRef::from(5);
            assert!(five.passes_cast(LpcType::Int(false), &txn));
            assert!(five.passes_cast(LpcType::Mixed(false), &txn));
            assert!(!five.passes_cast(LpcType::String(false), &txn));
            assert!(!five.passes_cast(LpcType::Float(false), &txn));
            assert!(!five.passes_cast(LpcType::Int(true), &txn));
        }

        #[test]
        fn an_array_passes_any_array_type() {
            let txn = TxnHandle::empty();
            let array = test_array_ref(&txn, LpcArray::new(vec![LpcRef::from(1)]));
            assert!(array.passes_cast(LpcType::String(true), &txn));
            assert!(array.passes_cast(LpcType::Mixed(true), &txn));
            assert!(!array.passes_cast(LpcType::String(false), &txn));
        }

        #[test]
        fn a_destructed_object_passes_every_cast() {
            let process = Arc::new(Process::new(Program::default()));
            let cell = VarId::new();
            process.cell.set(cell).unwrap();
            let live_ref = LpcRef::from(Arc::downgrade(&process));

            let live_txn = TxnHandle::empty();
            live_txn.with(|t| t.write_process(cell, process.clone()));
            assert!(!live_ref.passes_cast(LpcType::String(false), &live_txn));

            let destructing = TxnHandle::empty();
            destructing.with(|t| t.drop_var(cell));
            assert!(live_ref.passes_cast(LpcType::String(false), &destructing));
            assert!(live_ref.passes_cast(LpcType::Object(false), &destructing));
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

    mod bytes_ops {
        use super::*;

        #[test]
        fn bytes_add_slices_and_compare_by_content() {
            let txn = test_txn();
            let joined = test_bytes(b"ab").add(&test_bytes(b"c"), &txn).unwrap();
            assert_eq!(joined.as_bytes(), Some(b"abc".as_slice()));

            assert_eq!(test_bytes(b"abc"), test_bytes(b"abc"));
            assert!(test_bytes(b"a") < test_bytes(b"b"));
            assert!(test_bytes(b"\xff") > test_bytes(b"a"));
        }

        #[test]
        fn bytes_never_mix_with_a_string() {
            let txn = test_txn();
            for (a, b) in [
                (test_bytes(b"a"), LpcRef::from("b")),
                (LpcRef::from("b"), test_bytes(b"a")),
            ] {
                let err = a.add(&b, &txn).unwrap_err().to_string();
                assert!(err.contains("to_text"), "{err}");
            }
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

    mod array_sets {
        use std::sync::Arc;

        use lpc_rs_core::{lpc_path::LpcPath, null_mode::NullMode};
        use lpc_rs_utils::config::ConfigBuilder;

        use super::super::{LpcRef, test_array_ref};
        use crate::{
            interpreter::{
                lpc_array::LpcArray,
                object_space::ObjectSpace,
                process::Process,
                program::{Program, ProgramBuilder},
                stm::{
                    Changeset, Committer, Conflict, ObjectChange, Transaction, TxnHandle,
                    txn_insert_process, txn_undo_insert,
                },
                task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
                vm::Vm,
            },
            test_support::{permissive_master, run_prog_with_config},
        };

        async fn observe(code: &str, mode: NullMode) -> String {
            let config = crate::test_config_builder!()
                .null_mode(mode)
                .build()
                .unwrap();
            let task = run_prog_with_config(code, config.into()).await;
            task.result().unwrap().as_str().unwrap().to_owned()
        }

        fn counts(left: &LpcRef, right: &LpcRef, txn: &TxnHandle) -> [usize; 3] {
            [
                left.sub(right, txn),
                left.bitand(right, txn),
                left.bitor(right, txn),
            ]
            .map(|result| result.unwrap().with_array(txn, |a| a.len()).unwrap())
        }

        #[tokio::test]
        async fn stored_destroyed_references_match_the_selected_null_in_both_operand_directions() {
            let code = r#"
                string create() {
                    object o = clone_object("/clone_target");
                    mixed *a = ({o, 42, o});
                    mixed *b = ({nil});
                    destruct(o);
                    return sprintf("%d,%d;%d,%d,%d;%d,%d,%d;%d,%d",
                        a[0] == nil, member_array(nil, a),
                        sizeof(a - b), sizeof(a & b), sizeof(a | b),
                        sizeof(b - a), sizeof(b & a), sizeof(b | a), sizeof(a), sizeof(b));
                }
            "#;
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let result = observe(code, mode).await;
                assert_eq!(result, "1,0;1,2,3;0,1,2;3,1");
            }
        }

        #[tokio::test]
        async fn different_destroyed_objects_match_each_other_in_sets() {
            let code = r#"
                string create() {
                    object x = clone_object("/clone_target");
                    object y = clone_object("/clone_target");
                    mixed *a = ({x, 42, x});
                    mixed *b = ({y, y});
                    destruct(({x, y}));
                    return sprintf("%d,%d;%d,%d,%d;%d,%d,%d",
                        a[0] == b[0], member_array(b[0], a),
                        sizeof(a - b), sizeof(a & b), sizeof(a | b),
                        sizeof(b - a), sizeof(b & a), sizeof(b | a));
                }
            "#;
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let result = observe(code, mode).await;
                assert_eq!(result, "1,0;1,2,3;0,2,3");
            }
        }

        #[tokio::test]
        async fn references_inserted_after_destruction_match_the_selected_null() {
            let code = r#"
                string create() {
                    object o = clone_object("/clone_target");
                    destruct(o);
                    mixed *a = ({o, 42, o});
                    mixed *b = ({nil});
                    return sprintf("%d,%d,%d;%d,%d,%d",
                        sizeof(a - b), sizeof(a & b), sizeof(a | b),
                        sizeof(b - a), sizeof(b & a), sizeof(b | a));
                }
            "#;
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let result = observe(code, mode).await;
                assert_eq!(result, "1,2,3;0,1,2");
            }
        }

        #[tokio::test]
        async fn identical_stored_weak_references_still_match_before_and_after_destruction() {
            let code = r#"
                string create() {
                    object o = clone_object("/clone_target");
                    mixed *a = ({o, 42, o});
                    mixed *b = ({o});
                    string before = sprintf("%d,%d,%d", sizeof(a-b), sizeof(a&b), sizeof(a|b));
                    destruct(o);
                    return before + ";" + sprintf("%d,%d,%d", sizeof(a-b), sizeof(a&b), sizeof(a|b));
                }
            "#;
            for mode in [NullMode::Distinct, NullMode::Zero] {
                assert_eq!(observe(code, mode).await, "1,2,3;1,2,3");
            }
        }

        #[tokio::test]
        async fn numeric_set_matching_intentionally_remains_type_sensitive() {
            let code = r#"
                string create() {
                    mixed *a = ({1, 1, 9007199254740993});
                    mixed *b = ({1.0, 9007199254740992.0});
                    return sprintf("%d,%d,%d,%d;%d,%d,%d",
                        1 == 1.0, member_array(1.0, a),
                        9007199254740993 == 9007199254740992.0, member_array(9007199254740992.0, a),
                        sizeof(a-b), sizeof(a&b), sizeof(a|b));
                }
            "#;
            for mode in [NullMode::Distinct, NullMode::Zero] {
                assert_eq!(observe(code, mode).await, "1,0,0,-1;3,0,5");
            }
        }

        #[tokio::test]
        async fn content_and_identity_matching_preserve_collection_contracts() {
            let code = r#"
                class point { int x; }
                string create() {
                    mixed *inner = ({7});
                    mapping m = ([1: 2]);
                    class point c = new(class point, x: 7);
                    function f = sizeof;
                    mixed *a = ({"x", to_bytes("x", "UTF-8"), inner, m, c, f});
                    mixed *b = ({"x", to_bytes("x", "UTF-8"), ({7}), ([1: 2]), new(class point, x: 7)});
                    mixed *same = ({inner, m, c, f});
                    return sprintf("%d,%d,%d;%d,%d,%d;%d,%d",
                        sizeof(a-b), sizeof(a&b), sizeof(a|b),
                        sizeof(a-same), sizeof(a&same), sizeof(a|same),
                        sizeof(({}) - ({})), ({}) == ({}));
                }
            "#;
            for mode in [NullMode::Distinct, NullMode::Zero] {
                assert_eq!(observe(code, mode).await, "4,2,9;2,4,6;0,0");
            }
        }

        #[tokio::test]
        async fn order_duplicates_and_compound_assignment_preserve_original_arrays() {
            let code = r#"
                string create() {
                    int *a = ({3, 1, 3, 2, 1});
                    int *b = ({1, 4, 4});
                    int *minus = a; int *both = a; int *either = a;
                    minus -= b; both &= b; either |= b;
                    return implode(map(minus, to_string), ",") + ";" +
                        implode(map(both, to_string), ",") + ";" +
                        implode(map(either, to_string), ",") + ";" +
                        implode(map(a, to_string), ",") + ";" + implode(map(b, to_string), ",");
                }
            "#;
            for mode in [NullMode::Distinct, NullMode::Zero] {
                assert_eq!(
                    observe(code, mode).await,
                    "3,3,2;1,1;3,1,3,2,1,4,4;3,1,3,2,1;1,4,4"
                );
            }
        }

        #[tokio::test]
        async fn stored_global_array_matches_null_after_a_committed_destruct() {
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let config = crate::test_config_builder!()
                    .null_mode(mode)
                    .build()
                    .unwrap();
                let vm = Vm::new(config);
                permissive_master(&vm.global_state.object_space).await;
                let owner = vm
                    .initialize_process_from_code(
                        "/probe.c",
                        r#"
                    mixed *a;
                    void create() { object o = clone_object("/clone_target"); a = ({o,42,o}); }
                    void retire() { destruct(a[0]); }
                    string inspect() {
                        mixed *b = ({nil});
                        return sprintf("%d,%d;%d,%d,%d",
                            a[0] == nil, member_array(nil,a), sizeof(a-b), sizeof(a&b), sizeof(a|b));
                    }
                "#,
                    )
                    .await
                    .unwrap()
                    .context
                    .process;
                apply_function_by_name(
                    "retire",
                    &[],
                    owner.clone(),
                    TaskTemplate::from(vm.global_state.clone()),
                    Some(4000),
                )
                .await
                .unwrap()
                .unwrap();
                let result = apply_function_by_name(
                    "inspect",
                    &[],
                    owner,
                    TaskTemplate::from(vm.global_state.clone()),
                    Some(4000),
                )
                .await
                .unwrap()
                .unwrap();
                assert_eq!(result, LpcRef::from("1,0;1,2,3"));
            }
        }

        #[test]
        fn expired_weak_references_match_null_without_rewriting_stored_members() {
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let committer = Committer::with_null_mode(mode);
                let txn = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                let process = Arc::new(Process::new(Program::default()));
                let weak = LpcRef::from(Arc::downgrade(&process));
                drop(process);
                let a = test_array_ref(
                    &txn,
                    LpcArray::from_iter([weak.clone(), 42.into(), weak.clone()]),
                );
                let b = test_array_ref(&txn, LpcArray::from_iter([txn.nil()]));
                assert!(weak.eq_in(&txn.nil(), &txn));
                assert_eq!(counts(&a, &b, &txn), [1, 2, 3]);
                a.with_array(&txn, |stored| {
                    assert_eq!(stored[0], weak);
                    assert!(matches!(stored[0], LpcRef::Object(_)));
                })
                .unwrap();
                a.bitand(&b, &txn)
                    .unwrap()
                    .with_array(&txn, |kept| {
                        assert_eq!(kept[0], weak);
                        assert!(matches!(kept[0], LpcRef::Object(_)));
                    })
                    .unwrap();
            }
        }

        #[test]
        fn liveness_follows_pending_committed_aborted_and_old_snapshot_removal() {
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let mut committer = Committer::with_null_mode(mode);
                let space = Arc::new(ObjectSpace::default());
                let process = Arc::new(Process::new(
                    ProgramBuilder::default()
                        .filename(LpcPath::from("/victim.c"))
                        .build()
                        .unwrap(),
                ));
                ObjectSpace::insert_process_physical(&space, process.clone());
                let mut placement = Changeset::new(committer.current_version());
                placement
                    .object_changes
                    .push(ObjectChange::new(&process, true));
                committer.commit(placement).unwrap();
                let old = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                let weak = LpcRef::from(Arc::downgrade(&process));
                assert!(!weak.eq_in(&old.nil(), &old));
                let aborted = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                txn_undo_insert(&aborted, &space, &process);
                assert!(weak.eq_in(&aborted.nil(), &aborted));
                drop(aborted);
                let removal = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                assert!(!weak.eq_in(&removal.nil(), &removal));
                txn_undo_insert(&removal, &space, &process);
                assert!(weak.eq_in(&removal.nil(), &removal));
                committer
                    .commit(removal.with(|t| t.take_changeset()))
                    .unwrap();
                let fresh = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                for flush in [false, true] {
                    if flush {
                        space.apply_remove("/victim", &process);
                    }
                    assert!(!weak.eq_in(&old.nil(), &old));
                    assert!(weak.eq_in(&fresh.nil(), &fresh));
                    let a = test_array_ref(&fresh, LpcArray::from_iter([weak.clone()]));
                    let b = test_array_ref(&fresh, LpcArray::from_iter([fresh.nil()]));
                    assert_eq!(counts(&a, &b, &fresh), [0, 1, 1]);
                }
                assert!(matches!(
                    committer.commit(old.with(|t| t.take_changeset())),
                    Err(Conflict::ReadInvalidated { .. })
                ));
            }
        }

        #[tokio::test]
        async fn literal_null_values_follow_the_existing_set_rules() {
            let code = r#"
                string create() {
                    mixed *a = ({nil, 42, nil}); mixed *b = ({nil});
                    return sprintf("%d,%d,%d;%d,%d,%d",
                        sizeof(a-b), sizeof(a&b), sizeof(a|b),
                        sizeof(b-a), sizeof(b&a), sizeof(b|a));
                }
            "#;
            for mode in [NullMode::Distinct, NullMode::Zero] {
                assert_eq!(observe(code, mode).await, "1,2,3;0,1,2");
            }
        }

        #[tokio::test]
        async fn integer_zero_is_a_different_selector_in_distinct_mode() {
            let code = r#"
                string create() {
                    object o = clone_object("/clone_target");
                    mixed *a = ({o,42,o}); mixed *b = ({0});
                    destruct(o);
                    return sprintf("%d,%d;%d,%d,%d;%d,%d,%d",
                        a[0] == 0, member_array(0,a),
                        sizeof(a-b), sizeof(a&b), sizeof(a|b),
                        sizeof(b-a), sizeof(b&a), sizeof(b|a));
                }
            "#;
            assert_eq!(observe(code, NullMode::Distinct).await, "0,-1;3,0,4;1,0,4");
        }

        #[tokio::test]
        async fn null_matching_preserves_distinct_zero_and_type_sensitive_float_zero() {
            let code = r#"
                string create() {
                    object o = clone_object("/clone_target");
                    mixed *a = ({o, 0, 0.0, nil});
                    destruct(o);
                    return sprintf("%d,%d;%d,%d,%d",
                        sizeof(a-({nil})), sizeof(a-({0})),
                        sizeof(a&({nil})), sizeof(a&({0})), sizeof(a&({0.0})));
                }
            "#;
            assert_eq!(observe(code, NullMode::Distinct).await, "2,3;2,1,1");
            assert_eq!(observe(code, NullMode::Zero).await, "1,1;3,3,1");
        }

        fn placed_object(mode: NullMode) -> (Committer, Arc<ObjectSpace>, Arc<Process>) {
            let mut committer = Committer::with_null_mode(mode);
            let space = Arc::new(ObjectSpace::default());
            let process = Arc::new(Process::new(
                ProgramBuilder::default()
                    .filename(LpcPath::from("/victim.c"))
                    .build()
                    .unwrap(),
            ));
            let placement = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
            txn_insert_process(&placement, &space, &process);
            committer
                .commit(placement.with(|t| t.take_changeset()))
                .unwrap();
            (committer, space, process)
        }

        #[test]
        fn subtraction_tracks_liveness_and_conflicts_with_concurrent_destruction() {
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let (mut committer, space, process) = placed_object(mode);
                let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                let left = test_array_ref(
                    &reader,
                    LpcArray::from_iter([LpcRef::from(Arc::downgrade(&process))]),
                );
                let right = test_array_ref(&reader, LpcArray::from_iter([reader.nil()]));
                let result = left.sub(&right, &reader).unwrap();
                assert_eq!(result.with_array(&reader, |a| a.len()).unwrap(), 1);
                let removal = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                txn_undo_insert(&removal, &space, &process);
                committer
                    .commit(removal.with(|t| t.take_changeset()))
                    .unwrap();
                assert!(
                    matches!(committer.commit(reader.with(|t| t.take_changeset())),
                    Err(Conflict::ReadInvalidated { cell, .. }) if cell == *process.cell.get().unwrap())
                );
            }
        }

        #[test]
        fn intersection_tracks_liveness_and_conflicts_with_concurrent_destruction() {
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let (mut committer, space, process) = placed_object(mode);
                let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                let left = test_array_ref(
                    &reader,
                    LpcArray::from_iter([LpcRef::from(Arc::downgrade(&process))]),
                );
                let right = test_array_ref(&reader, LpcArray::from_iter([reader.nil()]));
                let result = left.bitand(&right, &reader).unwrap();
                assert_eq!(result.with_array(&reader, |a| a.len()).unwrap(), 0);
                let removal = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                txn_undo_insert(&removal, &space, &process);
                committer
                    .commit(removal.with(|t| t.take_changeset()))
                    .unwrap();
                assert!(
                    matches!(committer.commit(reader.with(|t| t.take_changeset())),
                    Err(Conflict::ReadInvalidated { cell, .. }) if cell == *process.cell.get().unwrap())
                );
            }
        }

        #[test]
        fn union_tracks_liveness_and_conflicts_with_concurrent_destruction() {
            for mode in [NullMode::Distinct, NullMode::Zero] {
                let (mut committer, space, process) = placed_object(mode);
                let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                let left = test_array_ref(
                    &reader,
                    LpcArray::from_iter([LpcRef::from(Arc::downgrade(&process))]),
                );
                let right = test_array_ref(&reader, LpcArray::from_iter([reader.nil()]));
                let result = left.bitor(&right, &reader).unwrap();
                assert_eq!(result.with_array(&reader, |a| a.len()).unwrap(), 2);
                let removal = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
                txn_undo_insert(&removal, &space, &process);
                committer
                    .commit(removal.with(|t| t.take_changeset()))
                    .unwrap();
                assert!(
                    matches!(committer.commit(reader.with(|t| t.take_changeset())),
                    Err(Conflict::ReadInvalidated { cell, .. }) if cell == *process.cell.get().unwrap())
                );
            }
        }
    }

    mod test_sub {
        use super::*;

        #[test]
        fn int_int() {
            let txn = test_txn();
            let result = LpcRef::from(456).sub(&LpcRef::from(123), &txn);
            assert_eq!(result.unwrap(), LpcRef::from(333));
        }

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
            let txn = test_txn();
            let int = LpcRef::from(8);
            let int2 = LpcRef::from(15);
            let result = int.bitand(&int2, &txn);
            if let Ok(LpcRef::Int(x)) = result {
                assert_eq!(x, 8)
            } else {
                panic!("no match")
            }
        }

        #[test]
        fn array_array() {
            let txn = test_txn();
            let to_ref = |i| LpcRef::Int(LpcInt(i));
            let v1 = vec![3, 1, 2, 1].into_iter().map(to_ref).collect::<Vec<_>>();
            let v2 = vec![1, 3, 9].into_iter().map(to_ref).collect::<Vec<_>>();
            let a1 = test_array_ref(&txn, LpcArray::new(v1));
            let a2 = test_array_ref(&txn, LpcArray::new(v2));

            let result = a1.bitand(&a2, &txn);
            let expected = vec![3, 1, 1].into_iter().map(to_ref).collect::<Vec<_>>();

            result
                .unwrap()
                .with_array(&txn, |a| {
                    assert_eq!(*a, expected);
                })
                .unwrap();
        }
    }

    mod test_or {
        use super::*;

        #[test]
        fn array_array() {
            let txn = test_txn();
            let to_ref = |i| LpcRef::Int(LpcInt(i));
            let v1 = vec![1, 2, 3, 3].into_iter().map(to_ref).collect::<Vec<_>>();
            let v2 = vec![1, 4, 4].into_iter().map(to_ref).collect::<Vec<_>>();
            let a1 = test_array_ref(&txn, LpcArray::new(v1));
            let a2 = test_array_ref(&txn, LpcArray::new(v2));

            let result = a1.bitor(&a2, &txn);
            let expected = vec![1, 2, 3, 3, 4, 4]
                .into_iter()
                .map(to_ref)
                .collect::<Vec<_>>();

            result
                .unwrap()
                .with_array(&txn, |a| {
                    assert_eq!(*a, expected);
                })
                .unwrap();
        }

        #[test]
        fn int_int() {
            let txn = test_txn();
            let int = LpcRef::from(7);
            let int2 = LpcRef::from(16);
            let result = int.bitor(&int2, &txn);
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

    #[test]
    fn float_divisor_tolerance_is_strict_at_epsilon() {
        for (divisor, rejected) in [
            (0.0, true),
            (-0.0, true),
            (BaseFloat::EPSILON / 2.0, true),
            (-BaseFloat::EPSILON / 2.0, true),
            (BaseFloat::EPSILON, false),
            (-BaseFloat::EPSILON, false),
            (2.0 * BaseFloat::EPSILON, false),
            (-2.0 * BaseFloat::EPSILON, false),
        ] {
            let left = LpcRef::from(1.0);
            let right = LpcRef::from(divisor);
            assert_eq!(left.div(&right).is_err(), rejected, "division by {divisor}");
            assert_eq!(
                left.rem(&right).is_err(),
                rejected,
                "remainder by {divisor}"
            );
        }
    }
    #[test]
    fn whole_word_shift_counts_preserve_the_original_integer() {
        let width = LpcIntInner::BITS as LpcIntInner;
        for value in [1, -1, LpcIntInner::MIN, LpcIntInner::MAX, 0x1234] {
            for count in [0, width, -width, 2 * width, -2 * width, LpcIntInner::MIN] {
                assert_eq!(int_shl(value, count), value, "left: {value}, {count}");
                assert_eq!(int_shr(value, count), value, "right: {value}, {count}");
                let left = LpcRef::from(value);
                let right = LpcRef::from(count);
                assert_eq!(left.shl(&right).unwrap(), LpcRef::from(value));
                assert_eq!(left.shr(&right).unwrap(), LpcRef::from(value));
            }
        }
    }
    #[test]
    fn representation_null_checks_distinguish_nonnull_values() {
        assert!(LpcRef::Nil.is_null());
        assert!(LpcRef::from(0).is_null());
        for value in [
            LpcRef::from(1),
            LpcRef::from(-1),
            LpcRef::from(0.0),
            LpcRef::from(""),
            LpcRef::from("value"),
            LpcRef::from(Vec::<u8>::new()),
        ] {
            assert!(!value.is_null(), "{value:?}");
        }
    }
    #[test]
    fn integer_or_preserves_shared_bits() {
        let txn = TxnHandle::empty();
        for value in [1, -1, LpcIntInner::MIN, LpcIntInner::MAX, 3, 6] {
            let value = LpcRef::from(value);
            assert_eq!(value.bitor(&value, &txn).unwrap(), value);
        }
        assert_eq!(
            LpcRef::from(3).bitor(&LpcRef::from(6), &txn).unwrap(),
            LpcRef::from(7)
        );
    }
    #[test]
    fn numeric_wrapper_conversions_preserve_their_variant_and_value() {
        for number in [LpcIntInner::MIN, -17, 0, 17, LpcIntInner::MAX] {
            let wrapped = LpcInt(number);
            assert_eq!(LpcRef::from(wrapped), LpcRef::Int(wrapped));
        }
        for number in [-3.5, -0.0, 0.0, 3.5] {
            let wrapped = LpcFloat::from(number);
            assert_eq!(LpcRef::from(wrapped), LpcRef::Float(wrapped));
        }
    }
    #[test]
    fn scalar_hashes_are_stable_and_contribute_value_information() {
        use std::{
            collections::{HashSet, hash_map::DefaultHasher},
            hash::{Hash, Hasher},
        };

        fn digest(value: &LpcRef) -> u64 {
            let mut hasher = DefaultHasher::new();
            value.hash(&mut hasher);
            hasher.finish()
        }
        let values = [
            LpcRef::Nil,
            LpcRef::from(0),
            LpcRef::from(1),
            LpcRef::from(-17),
            LpcRef::from(3.5),
            LpcRef::from(""),
            LpcRef::from("value"),
            LpcRef::from(vec![1u8, 2, 3]),
        ];
        let mut hashes = HashSet::new();
        for value in values {
            let hash = digest(&value);
            assert_eq!(hash, digest(&value.clone()));
            hashes.insert(hash);
        }
        assert!(
            hashes.len() > 1,
            "scalar values must not all hash to a constant"
        );
        assert_eq!(digest(&LpcRef::from(-0.0)), digest(&LpcRef::from(0.0)));
    }
    #[test]
    fn integer_references_compare_with_usize_by_value() {
        assert_eq!(LpcRef::from(0), 0usize);
        assert_eq!(LpcRef::from(17), 17usize);
        assert_ne!(LpcRef::from(17), 18usize);
        assert_ne!(LpcRef::from(-1), 0usize);
        assert_ne!(LpcRef::Nil, 0usize);
        assert_ne!(LpcRef::from(17.0), 17usize);
        assert_ne!(LpcRef::from("17"), 17usize);
    }
}
