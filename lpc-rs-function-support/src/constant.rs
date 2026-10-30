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

#[cfg(test)]
mod tests {
    use lpc_rs_core::class::ClassId;

    use super::*;

    #[test]
    fn defaults_follow_the_type_and_selected_null_mode() {
        let record = LpcType::Class(
            ClassId {
                source: "/records.h".into(),
                name: "Record".into(),
            },
            false,
        );
        for (type_, distinct) in [
            (LpcType::Int(false), LpcConstant::Int(0)),
            (LpcType::Float(false), LpcConstant::Float(0.0.into())),
            (LpcType::Void, LpcConstant::Nil),
            (LpcType::Nil, LpcConstant::Nil),
            (LpcType::String(false), LpcConstant::Nil),
            (LpcType::Bytes(false), LpcConstant::Nil),
            (LpcType::Object(false), LpcConstant::Nil),
            (LpcType::Mapping(false), LpcConstant::Nil),
            (LpcType::Function(false), LpcConstant::Nil),
            (LpcType::Mixed(false), LpcConstant::Nil),
            (LpcType::Int(true), LpcConstant::Nil),
            (LpcType::Float(true), LpcConstant::Nil),
            (LpcType::String(true), LpcConstant::Nil),
            (LpcType::Bytes(true), LpcConstant::Nil),
            (LpcType::Object(true), LpcConstant::Nil),
            (LpcType::Mapping(true), LpcConstant::Nil),
            (LpcType::Function(true), LpcConstant::Nil),
            (LpcType::Mixed(true), LpcConstant::Nil),
            (record, LpcConstant::Nil),
            (record.as_array(true), LpcConstant::Nil),
        ] {
            assert_eq!(
                LpcConstant::default_for(type_, NullMode::Zero),
                LpcConstant::Int(0)
            );
            assert_eq!(
                LpcConstant::default_for(type_, NullMode::Distinct),
                distinct,
                "{type_}"
            );
        }
    }

    #[test]
    fn display_preserves_literal_types_and_string_escaping() {
        for (constant, expected) in [
            (LpcConstant::Nil, "nil"),
            (LpcConstant::Int(-42), "-42"),
            (LpcConstant::Float(1.5.into()), "1.5"),
            (
                LpcConstant::String(Arc::new("hello\n\"world\"".into())),
                r#""hello\n\"world\"""#,
            ),
        ] {
            assert_eq!(constant.to_string(), expected);
        }
    }
}
