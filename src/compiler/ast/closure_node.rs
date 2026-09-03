use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_core::{ScopeId, function_flags::FunctionFlags, lpc_type::LpcType};
use lpc_rs_errors::{Result, span::Span};
use ustr::Ustr;

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait, SpannedNode},
        var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representation a function definition
#[derive(Debug, PartialEq, PartialOrd, Clone, Hash, Eq)]
pub struct ClosureNode {
    pub name: Ustr,
    pub return_type: LpcType,
    pub parameters: Option<Vec<VarInitNode>>,
    pub flags: FunctionFlags,
    pub body: Vec<AstNode>,
    pub span: Option<Span>,
    pub scope_id: Option<ScopeId>,
}

impl SpannedNode for ClosureNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for ClosureNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.enter_scope(&mut self.scope_id);
        let result = tree_walker.visit_closure(self).await;
        tree_walker.exit_scope();
        result
    }
}

impl Display for ClosureNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Statements have no source rendering, so only a lone expression body shows.
        match self.body.as_slice() {
            [AstNode::Expression(expression)] => write!(f, "(: {expression} :)"),
            _ => write!(f, "(: ... :)"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::ast::expression_node::ExpressionNode;

    fn closure(body: Vec<AstNode>) -> ClosureNode {
        ClosureNode {
            name: "closure-1".into(),
            return_type: LpcType::Mixed(false),
            parameters: None,
            flags: Default::default(),
            body,
            span: None,
            scope_id: None,
        }
    }

    #[test]
    fn a_lone_expression_body_displays_as_source() {
        let node = closure(vec![AstNode::Expression(ExpressionNode::from(1))]);

        assert_eq!(node.to_string(), "(: 1 :)");
    }

    #[test]
    fn a_statement_body_is_elided() {
        let node = closure(vec![
            AstNode::Expression(ExpressionNode::from(1)),
            AstNode::Expression(ExpressionNode::from(2)),
        ]);

        assert_eq!(node.to_string(), "(: ... :)");
    }
}
