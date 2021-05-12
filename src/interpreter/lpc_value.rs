use crate::{
    interpreter::lpc_ref::LpcRef, LpcFloat,
    LpcInt,
};
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::{
    collections::HashMap,
    fmt,
    fmt::Display,
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
