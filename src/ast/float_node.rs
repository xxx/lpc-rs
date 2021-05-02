use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
    parser::span::Span,
    LpcFloat,
};

use crate::errors::LpcError;

/// A node representing a float literal
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub struct FloatNode {
    pub value: LpcFloat,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl FloatNode {
    pub fn new(v: f64) -> Self {
        // avoid any potential issues
        let value: f64 = if v.is_nan() || v.is_infinite() {
            0 as f64
        } else {
            v
        };

        Self {
            value: LpcFloat::from(value),
            span: None,
        }
    }
}

impl SpannedNode for FloatNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for FloatNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), LpcError> {
        tree_walker.visit_float(self)
    }
}

impl Display for FloatNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "FloatNode[{}]", self.value)
    }
}
