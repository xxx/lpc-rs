use std::{
    fmt,
    fmt::{Display, Formatter},
};

use lpc_rs_core::{BaseFloat, LpcFloatInner};
use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
};

/// A node representing a float literal
#[derive(Hash, Debug, Copy, Clone, Eq, PartialOrd, PartialEq)]
pub struct FloatNode {
    pub value: LpcFloatInner,

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
            value: LpcFloatInner::from(value),
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
