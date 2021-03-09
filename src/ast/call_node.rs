use crate::{
    ast::{
        ast_node::{ASTNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    parser::span::Span,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Representation of a function call.
#[derive(Debug, PartialEq, Clone)]
pub struct CallNode {
    /// The list of function arguments being passed.
    pub arguments: Vec<ExpressionNode>,

    /// The name of the function being called
    pub name: String,

    /// The text span in the original file that this node represents. Used for error messages.
    pub span: Option<Span>,
}

impl SpannedNode for CallNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl ASTNodeTrait for CallNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_call(self)
    }
}

impl Display for CallNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "CallNode[{:?}]", self)
    }
}
