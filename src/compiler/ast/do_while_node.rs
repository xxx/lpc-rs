use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use indextree::NodeId;
use lpc_rs_errors::{Result, span::Span};

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representing a `do {} while` loop
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash, Eq)]
pub struct DoWhileNode {
    pub condition: ExpressionNode,
    pub body: Box<AstNode>,
    pub scope_id: Option<NodeId>,
    pub span: Option<Span>,
}

impl DoWhileNode {
    pub fn new(body: AstNode, condition: ExpressionNode, span: Option<Span>) -> Self {
        Self {
            condition,
            body: Box::new(body),
            scope_id: None,
            span,
        }
    }
}

#[async_trait]
impl AstNodeTrait for DoWhileNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.enter_scope(&mut self.scope_id);
        let result = tree_walker.visit_do_while(self).await;
        tree_walker.exit_scope();
        result
    }
}

impl Display for DoWhileNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "do {{ {} }} while ({})]", self.body, self.condition)
    }
}
