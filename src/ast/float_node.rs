use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
};
use lpc_rs_core::{BaseFloat, LpcFloat};
use lpc_rs_errors::Result;
use lpc_rs_errors::span::Span;

/// A node representing a float literal
#[derive(Hash, Debug, Copy, Clone, Eq, PartialOrd, PartialEq)]
pub struct FloatNode {
    pub value: LpcFloat,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl FloatNode {
    pub fn new(v: BaseFloat) -> Self {
        // avoid any potential issues
        let value: BaseFloat = if v.is_nan() || v.is_infinite() {
            BaseFloat::from(0)
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
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_float(self)
    }
}

impl Display for FloatNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
