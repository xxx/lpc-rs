use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_core::global_var_flags::GlobalVarFlags;
use lpc_rs_errors::{Result, span::Span};
use lpc_rs_function_support::symbol::Symbol;
use ustr::{Ustr, ustr};

use super::parsed_type::ParsedType;

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representing a variable definition, with optional initialization
#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Hash)]
pub struct VarInitNode {
    /// The LPC type.
    pub type_: ParsedType,

    /// The var name
    pub name: Ustr,

    /// The initialization value. When missing, defaults to 0.
    pub value: Option<ExpressionNode>,

    /// Is this var actually an array?
    pub array: bool,

    /// Is this var a global variable?
    pub global: bool,

    /// The text span in the original file that this node represents. Used for
    /// error messages.
    pub span: Option<Span>,

    /// The flags for this variable. Only applicable for globals.
    pub flags: Option<GlobalVarFlags>,

    /// Whether this parameter is declared `ref`.
    pub by_ref: bool,
}

impl VarInitNode {
    pub fn new(name: &str, type_: impl Into<ParsedType>) -> Self {
        Self {
            name: ustr(name),
            type_: type_.into(),
            value: None,
            array: false,
            global: false,
            span: None,
            flags: None,
            by_ref: false,
        }
    }

    /// Set a new type on this node, in a way that maintains the array state
    pub fn update_type(&mut self, new_type: impl Into<ParsedType>) {
        self.type_ = new_type.into().as_array(self.array);
    }
}

impl SpannedNode for VarInitNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for VarInitNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_var_init(self).await
    }
}

impl Display for VarInitNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let v = if let Some(x) = &self.value {
            format!(" = {x}")
        } else {
            String::from("")
        };

        write!(f, "{} {} = {}", self.type_, self.name, v)
    }
}

impl TryFrom<&mut VarInitNode> for Symbol {
    type Error = lpc_rs_errors::LpcError;
    fn try_from(node: &mut VarInitNode) -> Result<Self> {
        let s = Self::new(&node.name, node.type_.resolved()?);

        Ok(Self {
            span: node.span,
            flags: node.flags.unwrap_or_default(),
            ..s
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lpc_rs_core::lpc_type::LpcType;

    #[test]
    fn test_update_type_maintains_the_array_state() {
        let mut node = VarInitNode {
            type_: LpcType::Void.into(),
            name: ustr("marf"),
            value: None,
            array: true,
            global: false,
            span: None,
            flags: None,
            by_ref: false,
        };

        node.update_type(LpcType::Int(false));
        assert_eq!(node.type_, LpcType::Int(true));

        node.array = false;
        node.update_type(LpcType::String(true));
        assert_eq!(node.type_, LpcType::String(false));
    }
}
