use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_errors::{Result, lpc_error, span::Span};

use crate::compiler::ast::{parsed_type::ParsedType, var_init_node::VarInitNode};

/// A parsed parameter whose name is optional until a function body is present.
#[derive(Debug)]
pub enum Parameter {
    Named(Box<VarInitNode>),
    Unnamed {
        type_: ParsedType,
        by_ref: bool,
        span: Span,
    },
}

impl Parameter {
    pub fn into_named(self) -> Result<VarInitNode> {
        match self {
            Self::Named(node) => Ok(*node),
            Self::Unnamed { span, .. } => Err(lpc_error!(
                Some(span),
                "a parameter name is required in a function definition or closure"
            )),
        }
    }

    pub fn validate_variadic(&self) -> Result<()> {
        let (type_, by_ref, has_default, span) = match self {
            Self::Named(node) => (node.type_, node.by_ref, node.value.is_some(), node.span),
            Self::Unnamed {
                type_,
                by_ref,
                span,
            } => (*type_, *by_ref, false, Some(*span)),
        };
        let problem = if by_ref {
            Some("a named variadic parameter cannot be `ref`")
        } else if has_default {
            Some("a named variadic parameter cannot have a default value")
        } else if !type_.is_array() && type_ != LpcType::Mixed(false) {
            Some("a named variadic parameter must have type `mixed` or an array type")
        } else {
            None
        };
        if let Some(message) = problem {
            return Err(lpc_error!(span, "{message}"));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::ast::{expression_node::ExpressionNode, int_node::IntNode};

    #[test]
    fn named_and_unnamed_variadic_parameters_require_mixed_or_array_types() {
        for (type_, accepted) in [
            (LpcType::Mixed(false), true),
            (LpcType::Mixed(true), true),
            (LpcType::Int(true), true),
            (LpcType::String(true), true),
            (LpcType::Int(false), false),
            (LpcType::Float(false), false),
            (LpcType::String(false), false),
            (LpcType::Object(false), false),
        ] {
            for parameter in [
                Parameter::Named(Box::new(VarInitNode::new("values", type_))),
                Parameter::Unnamed {
                    type_: type_.into(),
                    by_ref: false,
                    span: Span::new(0, 0..1),
                },
            ] {
                let result = parameter.validate_variadic();
                assert_eq!(result.is_ok(), accepted, "{parameter:?}");
                if let Err(error) = result {
                    assert_eq!(
                        error.to_string(),
                        "a named variadic parameter must have type `mixed` or an array type"
                    );
                }
            }
        }
    }

    #[test]
    fn variadic_collectors_reject_references_and_default_values() {
        let mut reference = VarInitNode::new("values", LpcType::Mixed(false));
        reference.by_ref = true;
        let mut defaulted = VarInitNode::new("values", LpcType::Mixed(false));
        defaulted.value = Some(ExpressionNode::Int(IntNode::new(0)));
        for (parameter, expected) in [
            (
                Parameter::Named(Box::new(reference)),
                "a named variadic parameter cannot be `ref`",
            ),
            (
                Parameter::Unnamed {
                    type_: LpcType::Mixed(false).into(),
                    by_ref: true,
                    span: Span::new(0, 0..1),
                },
                "a named variadic parameter cannot be `ref`",
            ),
            (
                Parameter::Named(Box::new(defaulted)),
                "a named variadic parameter cannot have a default value",
            ),
        ] {
            assert_eq!(
                parameter.validate_variadic().unwrap_err().to_string(),
                expected
            );
        }
    }
}
