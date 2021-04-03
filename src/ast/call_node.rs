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
};
use crate::compiler::compiler_error::CompilerError;

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

impl AstNodeTrait for CallNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_call(self)
    }
}

impl Display for CallNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "CallNode[{:?}]", self)
    }
}
