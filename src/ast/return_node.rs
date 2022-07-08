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

/// A node representing a function return call.
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct ReturnNode {
    /// The value to return from the function.
    pub value: Option<ExpressionNode>,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl ReturnNode {
    pub fn new(value: Option<ExpressionNode>) -> Self {
        Self { value, span: None }
    }
}

impl SpannedNode for ReturnNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for ReturnNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_return(self)
    }
}

impl Display for ReturnNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = if let Some(x) = &self.value {
            x.to_string()
        } else {
            String::from("")
        };

        write!(f, "return {}", s)
    }
}
