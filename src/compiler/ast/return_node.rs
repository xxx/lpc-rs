use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representing a function return call.
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash, Eq)]
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

#[async_trait]
impl AstNodeTrait for ReturnNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_return(self).await
    }
}

impl Display for ReturnNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = if let Some(x) = &self.value {
            x.to_string()
        } else {
            String::from("")
        };

        write!(f, "return {s}")
    }
}
