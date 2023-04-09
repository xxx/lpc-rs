use std::{
    fmt,
    fmt::{Display, Formatter},
    rc::Rc,
};
use std::sync::Arc;

use educe::Educe;
use indexmap::IndexMap;
use lpc_rs_core::{BaseFloat, LpcFloat, LpcInt};
use qcell::QCell;

use crate::{
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        lpc_array::LpcArray,
        lpc_mapping::LpcMapping,
        lpc_ref::{HashedLpcRef, LpcRef},
        lpc_string::LpcString,
        process::Process,
    },
    util::qcell_debug,
};

/// An actual LPC value. These are stored in memory, and as constants.
/// They are only used in the interpreter.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum LpcValue {
    Float(LpcFloat),
    Int(LpcInt),
    String(LpcString),
    Array(LpcArray),
    Mapping(LpcMapping),
    Object(#[educe(Debug(method = "qcell_debug"))] Arc<QCell<Process>>),
    Function(FunctionPtr),
}

impl Eq for LpcValue {}

/// Extract the final value (or reference to such, in the case of non-`Copy`
/// value types) from an `LpcValue`. It's simply wrapping sugar to get the final
/// value out of an [`LpcValue`]. This macro is only for use in functions
/// returning `Result<T, LpcError>`.
///
/// # Arguments
/// `expr`: An LpcValue
/// `path`: The expected LpcValue subtype of value.
///
/// # Errors
/// Will immediately return an `Err` with an
/// [`LpcError`](lpc_rs_errors::LpcError) if `path` does not match `expr`'s
/// type.
#[macro_export]
macro_rules! try_extract_value {
    ( $x:expr, $y:path ) => {
        match &$x {
            $y(s) => s,
            x => {
                let msg = format!("Invalid LpcValue - received `{}`. This indicates a serious bug in the interpreter.", x);
                debug_assert!(false, "{}", msg);
                return Err(LpcError::new_bug(msg))
            }
        }
    };
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
            LpcValue::Object(_) => "object",
            LpcValue::Function(_) => "function",
        }
    }
}

impl Display for LpcValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LpcValue::Float(x) => write!(f, "{x}"),
            LpcValue::Int(x) => write!(f, "{x}"),
            LpcValue::String(x) => write!(f, "\"{x}\""),
            LpcValue::Array(x) => write!(f, "{x}"),
            LpcValue::Mapping(x) => write!(f, "{x}"),
            LpcValue::Object(_x) => write!(f, "< object (QCell data) >"),
            // LpcValue::Object(x) => write!(f, "< {} >", x.borrow()),
            LpcValue::Function(x) => write!(f, "{x}"),
        }
    }
}

impl From<LpcInt> for LpcValue {
    fn from(i: LpcInt) -> Self {
        Self::Int(i)
    }
}

impl From<BaseFloat> for LpcValue {
    fn from(f: BaseFloat) -> Self {
        Self::Float(LpcFloat::from(f))
    }
}

impl From<&str> for LpcValue {
    fn from(s: &str) -> Self {
        Self::String(LpcString::from(s))
    }
}

impl From<String> for LpcValue {
    fn from(s: String) -> Self {
        Self::String(LpcString::from(s))
    }
}

impl From<&String> for LpcValue {
    fn from(s: &String) -> Self {
        Self::String(LpcString::from(s))
    }
}

impl From<LpcString> for LpcValue {
    fn from(s: LpcString) -> Self {
        Self::String(s)
    }
}

impl From<Vec<LpcRef>> for LpcValue {
    fn from(v: Vec<LpcRef>) -> Self {
        Self::Array(LpcArray::new(v))
    }
}

impl From<&[LpcRef]> for LpcValue {
    fn from(v: &[LpcRef]) -> Self {
        Self::Array(LpcArray::new(v.to_vec()))
    }
}

impl<T> From<IndexMap<HashedLpcRef, LpcRef, T>> for LpcValue {
    fn from(m: IndexMap<HashedLpcRef, LpcRef, T>) -> Self {
        Self::Mapping(LpcMapping::new(m.into_iter().collect()))
    }
}

impl From<Arc<QCell<Process>>> for LpcValue {
    fn from(o: Arc<QCell<Process>>) -> Self {
        Self::Object(o)
    }
}

impl From<FunctionPtr> for LpcValue {
    fn from(f: FunctionPtr) -> Self {
        Self::Function(f)
    }
}

impl PartialEq for LpcValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LpcValue::Float(x), LpcValue::Float(y)) => x == y,
            (LpcValue::Int(x), LpcValue::Int(y)) => x == y,
            (LpcValue::String(x), LpcValue::String(y)) => x == y,
            (LpcValue::Array(x), LpcValue::Array(y)) => x == y,
            (LpcValue::Mapping(x), LpcValue::Mapping(y)) => {
                x.keys().collect::<Vec<_>>() == y.keys().collect::<Vec<_>>()
                    && x.values().collect::<Vec<_>>() == y.values().collect::<Vec<_>>()
            }
            (LpcValue::Object(x), LpcValue::Object(y)) => Arc::ptr_eq(x, y),
            _ => false,
        }
    }
}
