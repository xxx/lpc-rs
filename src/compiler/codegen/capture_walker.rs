//! Move explicit captures into their closure's creation scope before name resolution.

use async_trait::async_trait;
use indexmap::IndexMap;
use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_errors::{Result, lpc_bug, lpc_error};
use ustr::Ustr;

use super::tree_walker::{ContextHolder, Pass, TreeWalker, walk_closure};
use crate::compiler::{
    ast::{
        ast_node::AstNodeTrait, capture_node::CaptureNode, closure_node::ClosureNode,
        expression_node::ExpressionNode, var_init_node::VarInitNode, var_node::VarNode,
    },
    compilation_context::CompilationContext,
    diagnostics::Diagnostics,
};

pub struct CaptureWalker {
    context: CompilationContext,
    captures: Vec<IndexMap<Ustr, VarInitNode>>,
}

impl ContextHolder for CaptureWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl Pass for CaptureWalker {
    fn new(context: CompilationContext) -> Self {
        Self {
            context,
            captures: Vec::new(),
        }
    }

    fn diagnostics_mut(&mut self) -> &mut Diagnostics {
        &mut self.context.diagnostics
    }
}

#[async_trait]
impl TreeWalker for CaptureWalker {
    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        self.captures.push(IndexMap::new());
        let result = walk_closure(self, node).await;
        node.captures = self
            .captures
            .pop()
            .ok_or_else(|| lpc_bug!(node.span, "closure has no capture collection"))?
            .into_values()
            .collect();
        result
    }

    async fn visit_capture(&mut self, node: &mut CaptureNode) -> Result<()> {
        let Some(mut captures) = self.captures.pop() else {
            return Err(self.context.diagnostics.fail(lpc_error!(
                node.span,
                "`$()` can only be used within a closure"
            )));
        };

        let mut reference = VarNode::new(&node.name);
        reference.span = node.span;
        let mut expr = std::mem::replace(&mut *node.expr, ExpressionNode::Var(reference));
        let result = if let indexmap::map::Entry::Vacant(entry) = captures.entry(node.name) {
            // A capture's expression belongs to the creator, including any nested `$()`.
            let result = expr.visit(self).await;
            let mut init = VarInitNode::new(&node.name, LpcType::Mixed(false));
            init.value = Some(expr);
            init.span = node.span;
            entry.insert(init);
            result
        } else {
            Ok(())
        };
        self.captures.push(captures);
        result
    }
}
