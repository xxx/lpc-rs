//! An `expr...` argument: the array's elements are passed one per argument.

use std::fmt::{self, Display, Formatter};

use async_trait::async_trait;
use lpc_rs_errors::{Result, span::Span};

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// An array spread across a call's argument list.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct SpreadNode {
    /// The array expression.
    pub expr: Box<ExpressionNode>,
    /// The span of `expr...` in the source.
    pub span: Option<Span>,
}

impl SpannedNode for SpreadNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for SpreadNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_spread(self).await
    }
}

impl Display for SpreadNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}...", self.expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::{
        ast::{ast_node::AstNode, call_node::CallNode, var_init_node::VarInitNode},
        compilation_context::CompilationContext,
        lexer::LexWrapper,
    };
    use crate::lpc_parser;

    fn call_of(code: &str) -> CallNode {
        let mut context = CompilationContext::default();
        let mut prog_node = lpc_parser::ProgramParser::new()
            .parse(&mut context, LexWrapper::new(code, 0).triples())
            .unwrap();
        if let Some(AstNode::Decl(mut node)) = prog_node.body.pop()
            && let Some(VarInitNode { value, .. }) = node.initializations.pop()
            && let Some(ExpressionNode::Call(node)) = value
        {
            node
        } else {
            panic!("expected call node");
        }
    }

    #[test]
    fn a_trailing_ellipsis_makes_a_spread_argument() {
        let call = call_of("mixed m = g(a, xs...);");
        assert_eq!(call.arguments.len(), 2);
        assert!(matches!(call.arguments[0], ExpressionNode::Var(_)));
        let ExpressionNode::Spread(spread) = &call.arguments[1] else {
            panic!("expected a spread");
        };
        assert!(matches!(*spread.expr, ExpressionNode::Var(_)));
        assert_eq!(call.to_string(), "g(a ,xs...)");
    }

    #[test]
    fn a_spread_may_sit_anywhere_and_repeat() {
        let call = call_of("mixed m = g(xs..., 1, ys...);");
        let kinds: Vec<bool> = call
            .arguments
            .iter()
            .map(|a| matches!(a, ExpressionNode::Spread(_)))
            .collect();
        assert_eq!(kinds, vec![true, false, true]);
    }

    #[test]
    fn a_pointer_partial_list_does_not_spread() {
        let mut context = CompilationContext::default();
        let result = lpc_parser::ProgramParser::new().parse(
            &mut context,
            LexWrapper::new("mixed m = &g(xs...);", 0).triples(),
        );
        assert!(result.is_err());
    }
}
