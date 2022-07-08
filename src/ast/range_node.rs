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
    Result,
};
use lpc_rs_errors::span::Span;

/// Representation of a Range, with optional ends.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct RangeNode {
    /// Left-hand side
    pub l: Box<Option<ExpressionNode>>,

    /// Right-hand side
    pub r: Box<Option<ExpressionNode>>,

    /// The text span in the original file that this node represents. Used for error messages.
    pub span: Option<Span>,
}

impl RangeNode {
    pub fn new(l: Option<ExpressionNode>, r: Option<ExpressionNode>, span: Option<Span>) -> Self {
        Self {
            l: Box::new(l),
            r: Box::new(r),
            span,
        }
    }
}

impl SpannedNode for RangeNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for RangeNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_range(self)
    }
}

impl Display for RangeNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let stringify = |a: &Option<ExpressionNode>| -> String {
            if let Some(b) = a {
                b.to_string()
            } else {
                String::from("")
            }
        };

        let ls = stringify(&self.l);
        let rs = stringify(&self.r);

        write!(f, "{}..{}", ls, rs)
    }
}
