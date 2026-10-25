use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_core::{ScopeId, function_flags::FunctionFlags};
use lpc_rs_errors::{Result, span::Span};
use ustr::Ustr;

use super::parsed_type::ParsedType;

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
    pub return_type: ParsedType,
    pub parameters: Option<Vec<VarInitNode>>,
    /// The named collector, excluded from the fixed `parameters` and call arity.
    pub variadic: Option<Box<VarInitNode>>,
    pub flags: FunctionFlags,
    pub body: Vec<AstNode>,
    pub span: Option<Span>,
    pub scope_id: Option<ScopeId>,
    /// Hidden variables evaluated in the creator before entering the closure.
    pub captures: Vec<VarInitNode>,
    pub capture_scope_id: Option<ScopeId>,
}

impl SpannedNode for ClosureNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for ClosureNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        let has_captures = !self.captures.is_empty();
        if has_captures {
            tree_walker.enter_scope(&mut self.capture_scope_id);
            for capture in &mut self.captures {
                if let Err(error) = tree_walker.visit_capture_initializer(capture).await {
                    tree_walker.exit_scope();
                    return Err(error);
                }
            }
        }
        tree_walker.enter_scope(&mut self.scope_id);
        let result = tree_walker.visit_closure(self).await;
        tree_walker.exit_scope();
        if has_captures {
            tree_walker.exit_scope();
        }
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
    use crate::compiler::{
        ast::expression_node::ExpressionNode, compilation_context::CompilationContext,
        lexer::LexWrapper,
    };
    use lpc_rs_core::lpc_type::LpcType;

    fn closure(body: Vec<AstNode>) -> ClosureNode {
        ClosureNode {
            variadic: None,
            name: "closure-1".into(),
            return_type: LpcType::Mixed(false).into(),
            parameters: None,
            flags: Default::default(),
            body,
            span: None,
            scope_id: None,
            captures: Vec::new(),
            capture_scope_id: None,
        }
    }

    #[test]
    fn a_parsed_expression_body_retains_its_span_and_source_form() {
        let source = "function f = (: 1 :);";
        let mut context = CompilationContext::default();
        let mut program = crate::lpc_parser::ProgramParser::new()
            .parse(&mut context, LexWrapper::new(source, 4).triples())
            .unwrap();
        let Some(AstNode::Decl(mut declaration)) = program.body.pop() else {
            panic!("closure declaration");
        };
        let Some(ExpressionNode::Closure(node)) = declaration
            .initializations
            .pop()
            .and_then(|init| init.value)
        else {
            panic!("closure initializer");
        };
        assert_eq!(
            node.span(),
            Some(Span::new(
                4,
                source.find("(:").unwrap()..source.find(":)").unwrap() + 2
            ))
        );
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
