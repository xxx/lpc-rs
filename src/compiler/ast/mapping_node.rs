use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use itertools::Itertools;
use lpc_rs_errors::{Result, span::Span};

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

#[cfg(test)]
mod tests {
    use crate::{interpreter::lpc_ref::LpcRef, test_support::run_prog};

    #[tokio::test]
    async fn mapping_literal_keys_evaluate_each_expression() {
        let task = run_prog(
            r#"
            int count;
            int next() { return ++count; }
            int create() {
                mapping values = ([ next(): 1, next(): 2 ]);
                return sizeof(values) * 10 + count;
            }
        "#,
        )
        .await;
        assert_eq!(task.context.result().unwrap(), LpcRef::from(22));
    }

    #[tokio::test]
    async fn repeated_mapping_keys_preserve_value_expression_effects() {
        let task = run_prog(
            r#"
            int count;
            int next() { return ++count; }
            int create() {
                mapping values = ([ 1: next(), 1: next() ]);
                return values[1] * 10 + count;
            }
        "#,
        )
        .await;
        assert_eq!(task.context.result().unwrap(), LpcRef::from(22));
    }

    #[tokio::test]
    async fn mapping_literals_evaluate_pairs_in_order_and_keep_key_positions() {
        for (source, expected) in [
            (
                r#"
                    int count;
                    int next() { return ++count; }
                    int create() {
                        mapping values = ([ next(): next(), next(): next() ]);
                        return values[1] * 100 + values[3] * 10 + count;
                    }
                "#,
                244,
            ),
            (
                r#"
                    int create() {
                        mapping values = ([ 2: 20, 1: 10, 2: 22 ]);
                        int *order = keys(values);
                        return order[0] * 10 + order[1];
                    }
                "#,
                21,
            ),
        ] {
            let task = run_prog(source).await;
            assert_eq!(task.context.result().unwrap(), LpcRef::from(expected));
        }
    }
}
