//! `operator(op)`: an operator as a function value, compiled as the
//! positional closure it stands for — `operator(+)` is `(: $1 + $2 :)`.

use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use itertools::Itertools;
use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType};
use lpc_rs_errors::{Result, span::Span};
use ustr::Ustr;

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait, SpannedNode},
        binary_op_node::{BinaryOpNode, BinaryOperation},
        closure_node::ClosureNode,
        expression_node::ExpressionNode,
        unary_op_node::{UnaryOpNode, UnaryOperation},
        var_node::VarNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// An operator that `operator(...)` admits. The grammar's `OperatorSymbol`
/// rule is its only source, and carries CD's set.
#[derive(Hash, Debug, Copy, Clone, Eq, PartialOrd, PartialEq)]
pub enum Operator {
    Binary(BinaryOperation),
    Unary(UnaryOperation),
}

impl Operator {
    /// How many arguments the operator takes.
    pub fn arity(self) -> usize {
        match self {
            Operator::Binary(_) => 2,
            Operator::Unary(_) => 1,
        }
    }

    /// The closure body applying the operator to its positionals, spanned
    /// at the `operator(...)` text so a runtime error points there.
    fn body(self, span: Option<Span>) -> ExpressionNode {
        let positional = |name: &str| {
            let mut var = VarNode::new(name);
            var.span = span;
            ExpressionNode::Var(var)
        };
        match self {
            Operator::Binary(op) => ExpressionNode::BinaryOp(BinaryOpNode {
                l: Box::new(positional("$1")),
                r: Box::new(positional("$2")),
                op,
                span,
            }),
            Operator::Unary(op) => ExpressionNode::UnaryOp(UnaryOpNode {
                expr: Box::new(positional("$1")),
                op,
                is_post: false,
                span,
            }),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Binary(op) => write!(f, "{op}"),
            Operator::Unary(op) => write!(f, "{op}"),
        }
    }
}

/// `operator(op)`, or `&operator(op)(args)` with `args` bound at creation
/// like `&f(args)`.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct OperatorNode {
    pub op: Operator,

    /// The closure `op` stands for, named like any other closure.
    pub closure: ClosureNode,

    /// The partially applied arguments, `None` for the bare form; a `None`
    /// element is a hole filled at call time.
    pub arguments: Option<Vec<Option<ExpressionNode>>>,

    /// The span of the whole form in the original file
    pub span: Option<Span>,
}

impl OperatorNode {
    /// The node for `op`, its closure named `closure_name`.
    pub fn new(
        op: Operator,
        arguments: Option<Vec<Option<ExpressionNode>>>,
        closure_name: Ustr,
        span: Option<Span>,
    ) -> Self {
        let closure = ClosureNode {
            name: closure_name,
            return_type: LpcType::Mixed(false),
            parameters: None,
            flags: FunctionFlags::from(&["private"][..]),
            body: vec![AstNode::Expression(op.body(span))],
            span,
            scope_id: None,
        };
        Self {
            op,
            closure,
            arguments,
            span,
        }
    }
}

impl SpannedNode for OperatorNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for OperatorNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_operator(self).await
    }
}

impl Display for OperatorNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.arguments {
            None => write!(f, "operator({})", self.op),
            Some(args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.as_ref().map_or(String::new(), ToString::to_string))
                    .join(", ");
                write!(f, "&operator({})({args})", self.op)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_binary_operator_takes_two_and_a_unary_one() {
        assert_eq!(Operator::Binary(BinaryOperation::Add).arity(), 2);
        assert_eq!(Operator::Unary(UnaryOperation::BitwiseNot).arity(), 1);
    }

    #[test]
    fn the_bare_and_partial_forms_display_as_written() {
        let bare = OperatorNode::new(
            Operator::Binary(BinaryOperation::Index),
            None,
            "closure-0".into(),
            None,
        );
        assert_eq!(bare.to_string(), "operator([])");

        let partial = OperatorNode::new(
            Operator::Binary(BinaryOperation::Sub),
            Some(vec![None, Some(ExpressionNode::from(1))]),
            "closure-1".into(),
            None,
        );
        assert_eq!(partial.to_string(), "&operator(-)(, 1)");
    }

    #[test]
    fn the_closure_is_the_operation_over_the_positionals() {
        let node = OperatorNode::new(
            Operator::Unary(UnaryOperation::Bang),
            None,
            "closure-0".into(),
            None,
        );
        assert_eq!(node.closure.name.as_str(), "closure-0");
        assert_eq!(node.closure.to_string(), "(: !$1 :)");
        assert!(node.closure.flags.private());
    }
}
