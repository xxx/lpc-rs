use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
    parser::span::Span,
};

use crate::errors::LpcError;
use std::hash::{Hash, Hasher};

/// A node representing a float literal
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct FloatNode {
    pub value: f64,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl FloatNode {
    pub fn new(v: f64) -> Self {
        // avoid any potential issues
        let value: f64 = if v.is_nan() {
            0 as f64
        } else {
            v
        };

        Self { value, span: None }
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

/// We implement this to allow any [`ExpressionNode`] to be used as a mapping key,
/// but seriously, think very hard before using a float as one.
impl Hash for FloatNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.to_bits().hash(state)
    }
}

impl Eq for FloatNode { }
