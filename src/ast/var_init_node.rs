use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
    parser::span::Span,
    semantic::lpc_type::LpcType,
};
use crate::compiler::compiler_error::CompilerError;
use crate::errors::LpcError;

/// A node representing a variable definition, with optional initialization
#[derive(Debug, Clone, PartialEq)]
pub struct VarInitNode {
    /// The LPC type.
    pub type_: LpcType,

    /// The var name
    pub name: String,

    /// The initialization value. When missing, defaults to 0.
    pub value: Option<ExpressionNode>,

    /// Is this var actually an array?
    pub array: bool,

    /// Is this var a global variable?
    pub global: bool,

    /// The text span in the original file that this node represents. Used for error messages.
    pub span: Option<Span>,
}

impl VarInitNode {
    pub fn new(name: &str, type_: LpcType) -> Self {
        Self {
            name: name.to_string(),
            type_,
            value: None,
            array: false,
            global: false,
            span: None,
        }
    }

    /// Set a new type on this node, in a way that maintains the array state
    pub fn update_type(&mut self, new_type: LpcType) {
        let updated = match new_type {
            LpcType::Void => LpcType::Void,
            LpcType::Int(_) => LpcType::Int(self.array),
            LpcType::String(_) => LpcType::String(self.array),
            LpcType::Float(_) => LpcType::Float(self.array),
            LpcType::Object(_) => LpcType::Object(self.array),
            LpcType::Mapping(_) => LpcType::Mapping(self.array),
            LpcType::Mixed(_) => LpcType::Mixed(self.array),
            LpcType::Union(x) => LpcType::Union(x),
        };

        self.type_ = updated;
    }
}

impl SpannedNode for VarInitNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for VarInitNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), LpcError> {
        tree_walker.visit_var_init(self)
    }
}

impl Display for VarInitNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "VarInitNode[{} {} {:?}]",
            self.type_, self.name, self.value
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_update_type_maintains_the_array_state() {
        let mut node = VarInitNode {
            type_: LpcType::Void,
            name: "marf".to_string(),
            value: None,
            array: true,
            global: false,
            span: None,
        };

        node.update_type(LpcType::Int(false));
        assert_eq!(node.type_, LpcType::Int(true));

        node.array = false;
        node.update_type(LpcType::String(true));
        assert_eq!(node.type_, LpcType::String(false));
    }
}
