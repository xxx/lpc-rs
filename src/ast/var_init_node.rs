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
    semantic::global_var_flags::GlobalVarFlags,
};
use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_errors::Result;
use lpc_rs_errors::span::Span;

/// A node representing a variable definition, with optional initialization
#[derive(Debug, Clone, PartialOrd, PartialEq)]
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

    /// The flags for this variable. Only applicable for globals.
    pub flags: Option<GlobalVarFlags>,
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
            flags: None,
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
            LpcType::Function(_) => LpcType::Function(self.array),
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
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_var_init(self)
    }
}

impl Display for VarInitNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let v = if let Some(x) = &self.value {
            format!(" = {}", x)
        } else {
            String::from("")
        };

        write!(f, "{} {} = {}", self.type_, self.name, v)
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
            flags: None,
        };

        node.update_type(LpcType::Int(false));
        assert_eq!(node.type_, LpcType::Int(true));

        node.array = false;
        node.update_type(LpcType::String(true));
        assert_eq!(node.type_, LpcType::String(false));
    }
}
