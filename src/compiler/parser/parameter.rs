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
        } else if !type_.is_array() {
            Some("a named variadic parameter must have an array type")
        } else {
            None
        };
        if let Some(message) = problem {
            return Err(lpc_error!(span, "{message}"));
        }
        Ok(())
    }
}
