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

/// A node representing an array literal
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct ArrayNode {
    pub value: Vec<ExpressionNode>,

    /// The full span of all expressions in the array - from the left side of
    /// the first item to the right side of the last
    pub span: Option<Span>,
}

impl ArrayNode {
    /// Construct a new `ArrayNode`. The `span` will be calculated
    /// as the span from the start of the first item, to the end of
    /// the last.
    pub fn new(value: Vec<ExpressionNode>) -> Self {
        let span = if value.is_empty() {
            None
        } else if let (Some(node1), Some(node2)) = (value[0].span(), value.last().unwrap().span()) {
            Some(Span {
                // #includes can make `file_id` differ between the nodes. We just take the first.
                file_id: node1.file_id,
                l: node1.l,
                r: node2.r,
            })
        } else {
            None
        };

        Self { value, span }
    }
}

impl SpannedNode for ArrayNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for ArrayNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_array(self).await
    }
}

impl Display for ArrayNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .value
            .iter()
            .map(|item| format!("{item}"))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "({{ {s} }})")
    }
}
