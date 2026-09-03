use std::{
    fmt::{Display, Formatter},
    sync::Arc,
};

use lpc_rs_core::{LpcFloatInner, LpcIntInner};
use lpc_rs_utils::lpc_string::LpcString;

/// One entry of a function's constant pool: a literal built once at compile
/// time, read through a `RegisterVariant::Constant` operand.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LpcConstant {
    Int(LpcIntInner),
    Float(LpcFloatInner),
    String(Arc<LpcString>),
}

impl Display for LpcConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LpcConstant::Int(x) => write!(f, "{x}"),
            LpcConstant::Float(x) => write!(f, "{x}"),
            LpcConstant::String(x) => write!(f, "{:?}", x.to_str()),
        }
    }
}
