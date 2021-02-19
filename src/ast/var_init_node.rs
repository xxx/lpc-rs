use crate::{
    ast::{ast_node::ASTNodeTrait, expression_node::ExpressionNode},
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    parser::span::Span,
    semantic::lpc_type::LPCType,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A node representing a variable definition, with optional initialization
#[derive(Debug, Clone, PartialEq)]
pub struct VarInitNode {
    /// The LPC type.
    pub type_: LPCType,

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
    pub fn new(name: &str, type_: LPCType) -> Self {
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
    pub fn update_type(&mut self, new_type: LPCType) {
        let updated = match new_type {
            LPCType::Void => LPCType::Void,
            LPCType::Int(_) => LPCType::Int(self.array),
            LPCType::String(_) => LPCType::String(self.array),
            LPCType::Float(_) => LPCType::Float(self.array),
            LPCType::Object(_) => LPCType::Object(self.array),
            LPCType::Mapping(_) => LPCType::Mapping(self.array),
            LPCType::Mixed(_) => LPCType::Mixed(self.array),
            LPCType::Union(x) => LPCType::Union(x),
        };

        self.type_ = updated;
    }
}

impl ASTNodeTrait for VarInitNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
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
            type_: LPCType::Void,
            name: "marf".to_string(),
            value: None,
            array: true,
            global: false,
            span: None,
        };

        node.update_type(LPCType::Int(false));
        assert_eq!(node.type_, LPCType::Int(true));

        node.array = false;
        node.update_type(LPCType::String(true));
        assert_eq!(node.type_, LPCType::String(false));
    }
}
