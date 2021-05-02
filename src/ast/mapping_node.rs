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
    errors::LpcError,
    parser::span::Span,
};
use std::hash::{Hash, Hasher};

/// A node representing an array literal
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MappingNode {
    pub value: Vec<(ExpressionNode, ExpressionNode)>,

    /// The full span of the mapping declaration - from the left side of the first item
    /// to the right side of the last
    pub span: Option<Span>,
}

impl MappingNode {
    /// Construct a new `MappingNode`. The `span` will be calculated
    /// as the span from the start of the first item, to the end of
    /// the last.
    pub fn new(value: Vec<(ExpressionNode, ExpressionNode)>, span: Option<Span>) -> Self {
        // go in reverse, so later-defined duplicate keys are the ones that stay
        // let value = vec.into_iter().rev().unique_by(|(key, _)| key).collect();
        Self { value, span }
    }
}

impl SpannedNode for MappingNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for MappingNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), LpcError> {
        tree_walker.visit_mapping(self)
    }
}

impl Display for MappingNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .value
            .iter()
            .map(|(key, value)| format!("{}: {}", key, value))
            .collect::<Vec<_>>()
            .join(",\n");
        write!(f, "MappingNode[{}]", s)
    }
}

impl Hash for MappingNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        "MappingNode".hash(state);

        self.
            value
            .iter()
            .for_each(|(key, value)| {
                key.hash(state);
                value.hash(state)
            })
    }
}
