//! The case labels of one `switch` body, found before the body is emitted
//! so the tests can precede it.

use async_trait::async_trait;
use lpc_rs_errors::Result;

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait},
        closure_node::ClosureNode,
        expression_node::ExpressionNode,
        label_node::LabelNode,
        switch_node::SwitchNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// The case expressions of `body`'s labels in visit order, `None` for
/// `default`; a nested `switch` owns its own, and a closure is another
/// function.
pub(super) async fn collect_cases(body: &mut AstNode) -> Result<Vec<Option<ExpressionNode>>> {
    let mut collector = CaseCollector { cases: vec![] };
    body.visit(&mut collector).await?;
    Ok(collector.cases)
}

struct CaseCollector {
    cases: Vec<Option<ExpressionNode>>,
}

#[async_trait]
impl TreeWalker for CaseCollector {
    async fn visit_label(&mut self, node: &mut LabelNode) -> Result<()> {
        self.cases.push(node.case.clone());
        Ok(())
    }

    async fn visit_switch(&mut self, _node: &mut SwitchNode) -> Result<()> {
        Ok(())
    }

    async fn visit_closure(&mut self, _node: &mut ClosureNode) -> Result<()> {
        Ok(())
    }
}
