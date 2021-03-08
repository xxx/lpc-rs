use crate::{
    ast::{ast_node::ASTNodeTrait, expression_node::ExpressionNode},
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    parser::span::Span,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};
use crate::ast::ast_node::SpannedNode;

/// Representation of a Range, with optional ends.
#[derive(Debug, Clone, PartialEq)]
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

impl ASTNodeTrait for RangeNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_range(self)
    }
}

impl Display for RangeNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "RangeNode[{:?}]", self)
    }
}
