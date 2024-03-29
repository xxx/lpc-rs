use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use itertools::Itertools;
use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representing an array literal
#[derive(Hash, Debug, Clone, Eq, PartialEq, PartialOrd)]
pub struct MappingNode {
    pub value: Vec<(ExpressionNode, ExpressionNode)>,

    /// The full span of the mapping declaration - from the left side of the
    /// first item to the right side of the last
    pub span: Option<Span>,
}

impl MappingNode {
    /// Construct a new `MappingNode`. The `span` will be calculated
    /// as the span from the start of the first item, to the end of
    /// the last.
    pub fn new(value: Vec<(ExpressionNode, ExpressionNode)>, span: Option<Span>) -> Self {
        Self { value, span }
    }
}

impl SpannedNode for MappingNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for MappingNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_mapping(self).await
    }
}

impl Display for MappingNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .value
            .iter()
            .map(|(key, value)| format!("{key}: {value}"))
            .join(", ");
        write!(f, "([ {s} ])")
    }
}
