use std::{
    fmt::{Display, Formatter},
    sync::Arc,
};

use lpc_rs_core::{LpcFloatInner, LpcIntInner, lpc_type::LpcType, null_mode::NullMode};
use lpc_rs_utils::lpc_string::LpcString;

/// One entry of a function's constant pool: a literal built once at compile
/// time, read through a `RegisterVariant::Constant` operand.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LpcConstant {
    Nil,
    Int(LpcIntInner),
    Float(LpcFloatInner),
    String(Arc<LpcString>),
}

impl Display for LpcConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LpcConstant::Nil => f.write_str("nil"),
            LpcConstant::Int(x) => write!(f, "{x}"),
            LpcConstant::Float(x) => write!(f, "{x}"),
            LpcConstant::String(x) => write!(f, "{:?}", x.to_str()),
        }
    }
}

impl LpcConstant {
    /// The initial value of a declaration in the selected mode.
    pub fn default_for(type_: LpcType, mode: NullMode) -> Self {
        if mode == NullMode::Zero {
            return Self::Int(0);
        }
        match type_ {
            LpcType::Int(false) => Self::Int(0),
            LpcType::Float(false) => Self::Float(0.0.into()),
            _ => Self::Nil,
        }
    }
}
