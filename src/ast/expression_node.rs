use crate::ast::ast_node::{ASTNodeTrait, ASTNode};
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::int_node::IntNode;
use crate::ast::call_node::CallNode;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::var_node::VarNode;
use crate::ast::assignment_node::AssignmentNode;
use crate::ast::string_node::StringNode;
use crate::parser::span::Span;
use crate::ast::array_node::ArrayNode;
use crate::errors::compiler_error::CompilerError;

/// A wrapper node for anything that can be considered an expression
/// (i.e. an operation that returns a value)
#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionNode {
    Assignment(AssignmentNode),
    BinaryOp(BinaryOpNode),
    Call(CallNode),
    Int(IntNode),
    String(StringNode),
    Var(VarNode),
    Array(ArrayNode)
}

impl ExpressionNode {
    pub fn span(&self) -> Option<Span> {
        match self {
            ExpressionNode::Assignment(node) => node.span,
            ExpressionNode::BinaryOp(node) => node.span,
            ExpressionNode::Call(node) => node.span,
            ExpressionNode::Int(node) => node.span,
            ExpressionNode::String(node) => node.span,
            ExpressionNode::Var(node) => node.span,
            ExpressionNode::Array(node) => node.span
        }
    }
}

macro_rules! delegated_traits {
    ( $( $x:path ),+ ) => {
        impl ASTNodeTrait for ExpressionNode {
            fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
                match self {
                $(
                    $x(y) => y.visit(tree_walker),
                )*
                }
            }
        }

        impl Clone for ExpressionNode {
            fn clone(&self) -> Self {
                match self {
                $(
                    $x(y) => $x((*y).clone()),
                )*
                }
            }
        }

        impl Display for ExpressionNode {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                match self {
                $(
                    $x(y) => write!(f, "{}", y),
                )*
                }
            }
        }
    };
}

delegated_traits!(
    ExpressionNode::Assignment,
    ExpressionNode::BinaryOp,
    ExpressionNode::Int,
    ExpressionNode::Call,
    ExpressionNode::Var,
    ExpressionNode::String,
    ExpressionNode::Array
);

impl From<BinaryOpNode> for ExpressionNode {
    fn from(node: BinaryOpNode) -> Self {
        Self::BinaryOp(node)
    }
}

impl From<IntNode> for ExpressionNode {
    fn from(node: IntNode) -> Self {
        Self::Int(node)
    }
}

impl From<VarNode> for ExpressionNode {
    fn from(node: VarNode) -> Self {
        Self::Var(node)
    }
}

impl From<AssignmentNode> for ExpressionNode {
    fn from(node: AssignmentNode) -> Self {
        Self::Assignment(node)
    }
}

impl From<CallNode> for ExpressionNode {
    fn from(node: CallNode) -> Self {
        Self::Call(node)
    }
}

impl From<ASTNode> for ExpressionNode {
    fn from(node: ASTNode) -> Self {
        match node {
            ASTNode::Expression(x) => x,
            x => panic!("unimplemented From<ASTNode> for ExpressionNode arm: {:?}", x)
        }
    }
}

impl From<i64> for ExpressionNode {
    fn from(value: i64) -> Self {
        Self::Int(IntNode::new(value))
    }
}

impl From<&str> for ExpressionNode {
    fn from(value: &str) -> Self {
        Self::String(StringNode { value: String::from(value), span: None })
    }
}

impl From<Vec<ExpressionNode>> for ExpressionNode {
    fn from(value: Vec<ExpressionNode>) -> Self {
        Self::Array(ArrayNode { value, span: None })
    }
}

impl From<Vec<&str>> for ExpressionNode {
    fn from(vec: Vec<&str>) -> Self {
        let value = vec.iter().map(|i| ExpressionNode::from(*i)).collect::<Vec<_>>();
        Self::Array(ArrayNode { value, span: None })
    }
}

impl From<Vec<i64>> for ExpressionNode {
    fn from(vec: Vec<i64>) -> Self {
        let value = vec.iter().map(|i| ExpressionNode::from(*i)).collect::<Vec<_>>();
        Self::Array(ArrayNode { value, span: None })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::binary_op_node::BinaryOperation;
    use crate::ast::assignment_node::AssignmentOperation;

    #[test]
    fn test_from_binary_op_node() {
        let node = BinaryOpNode {
            l: Box::new(IntNode::new(666).into()),
            r: Box::new(IntNode::new(324).into()),
            op: BinaryOperation::Add,
            span: None
        };

        let clone = node.clone();

        assert_eq!(ExpressionNode::from(node), ExpressionNode::BinaryOp(clone));
    }

    #[test]
    fn test_from_int_node() {
        let node = IntNode::new(666);

        let clone = node.clone();

        assert_eq!(ExpressionNode::from(node), ExpressionNode::Int(clone));
    }

    #[test]
    fn test_from_assignment_node() {
        let node = AssignmentNode {
            lhs: Box::new(VarNode::new("adsf").into()),
            rhs: Box::new(IntNode::new(324).into()),
            op: AssignmentOperation::Simple,
            span: None
        };

        let clone = node.clone();

        assert_eq!(ExpressionNode::from(node), ExpressionNode::Assignment(clone));
    }

    #[test]
    fn test_from_call_node() {
        let node = CallNode {
            arguments: vec![],
            name: "foo".to_string(),
            span: None
        };

        let clone = node.clone();

        assert_eq!(ExpressionNode::from(node), ExpressionNode::Call(clone));
    }

    mod from_ast_node {
        use crate::ast::expression_node::ExpressionNode;
        use crate::ast::int_node::IntNode;
        use crate::ast::ast_node::ASTNode;
        use crate::ast::program_node::ProgramNode;

        #[test]
        fn test_from_ast_node_is_ok_for_expressions() {
            let expression_node = ExpressionNode::Int(IntNode::new(666));
            let clone = expression_node.clone();
            let ast_node = ASTNode::Expression(expression_node);

            assert_eq!(ExpressionNode::from(ast_node), clone);
        }

        #[test]
        #[should_panic]
        fn test_from_ast_node_is_panics_for_non_expressions() {
            let node = ProgramNode::default();
            let ast_node = ASTNode::Program(node);

            ExpressionNode::from(ast_node);
        }
    }

    #[test]
    fn test_from_int() {
        let i = 666;

        assert_eq!(ExpressionNode::from(i), ExpressionNode::Int(IntNode { value: i, span: None }));
    }

    #[test]
    fn test_from_str() {
        let s = "hello";

        assert_eq!(
            ExpressionNode::from(s),
            ExpressionNode::String(StringNode { value: String::from(s), span: None })
        );
    }

    #[test]
    fn test_from_expression_node_vec() {
        let vec = vec![
            ExpressionNode::from(123),
            ExpressionNode::from(31),
            ExpressionNode::from(-4567),
            ExpressionNode::from(8238),
        ];


        assert_eq!(
            ExpressionNode::from(vec.to_vec()),
            ExpressionNode::Array(ArrayNode { value: vec, span: None })
        );
    }

    #[test]
    fn test_from_str_vec() {
        let vec = vec![
            "foo",
            "asdfas",
            "asdkj",
            "as",
        ];

        let value = vec.iter().map(|i| ExpressionNode::from(*i)).collect::<Vec<_>>();
        assert_eq!(
            ExpressionNode::from(vec.to_vec()),
            ExpressionNode::Array(ArrayNode { value, span: None })
        );
    }

    #[test]
    fn test_from_int_vec() {
        let vec = vec![
            123,
            31,
            -4567,
            8238,
        ];

        let value = vec.iter().map(|i| ExpressionNode::from(*i)).collect::<Vec<_>>();
        assert_eq!(
            ExpressionNode::from(vec.to_vec()),
            ExpressionNode::Array(ArrayNode { value, span: None })
        );
    }
}
