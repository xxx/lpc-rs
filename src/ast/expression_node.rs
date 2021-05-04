use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{
        array_node::ArrayNode,
        assignment_node::AssignmentNode,
        ast_node::{AstNode, AstNodeTrait, SpannedNode},
        binary_op_node::BinaryOpNode,
        call_node::CallNode,
        comma_expression_node::CommaExpressionNode,
        float_node::FloatNode,
        int_node::IntNode,
        mapping_node::MappingNode,
        range_node::RangeNode,
        string_node::StringNode,
        var_node::VarNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::LpcError,
    parser::span::Span,
    LpcInt,
};
use std::collections::HashMap;

/// A wrapper node for anything that can be considered an expression
/// (i.e. an operation that returns a value)
#[derive(Hash, Debug, Eq, PartialEq)]
pub enum ExpressionNode {
    Assignment(AssignmentNode),
    BinaryOp(BinaryOpNode),
    Call(CallNode),
    CommaExpression(CommaExpressionNode),
    Float(FloatNode),
    Int(IntNode),
    Range(RangeNode),
    String(StringNode),
    Var(VarNode),
    Array(ArrayNode),
    Mapping(MappingNode),
}

/// A convenience helper to get the first `span` we can find in a list of nodes.
/// Returns 0 if no spans are found.
///
/// # Arguments
/// `nodes` - A reference to a slice of Expression nodes.
pub fn first_span(nodes: &[&ExpressionNode]) -> Span {
    for node in nodes {
        if let Some(span) = node.span() {
            return span;
        }
    }

    Span::new(0, 0..0)
}

impl SpannedNode for ExpressionNode {
    fn span(&self) -> Option<Span> {
        match self {
            ExpressionNode::Assignment(node) => node.span,
            ExpressionNode::BinaryOp(node) => node.span,
            ExpressionNode::Call(node) => node.span,
            ExpressionNode::CommaExpression(node) => node.span,
            ExpressionNode::Float(node) => node.span,
            ExpressionNode::Int(node) => node.span,
            ExpressionNode::Range(node) => node.span,
            ExpressionNode::String(node) => node.span,
            ExpressionNode::Var(node) => node.span,
            ExpressionNode::Array(node) => node.span,
            ExpressionNode::Mapping(node) => node.span,
        }
    }
}

macro_rules! delegated_traits {
    ( $( $x:path ),+ ) => {
        impl AstNodeTrait for ExpressionNode {
            fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), LpcError> {
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
    ExpressionNode::Call,
    ExpressionNode::CommaExpression,
    ExpressionNode::Float,
    ExpressionNode::Int,
    ExpressionNode::Range,
    ExpressionNode::String,
    ExpressionNode::Var,
    ExpressionNode::Array,
    ExpressionNode::Mapping
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

impl From<RangeNode> for ExpressionNode {
    fn from(node: RangeNode) -> Self {
        Self::Range(node)
    }
}

// TODO: this should be TryInto
impl From<AstNode> for ExpressionNode {
    fn from(node: AstNode) -> Self {
        match node {
            AstNode::Expression(x) => x,
            x => panic!(
                "unimplemented From<AstNode> for ExpressionNode arm: {:?}",
                x
            ),
        }
    }
}

impl From<LpcInt> for ExpressionNode {
    fn from(value: LpcInt) -> Self {
        Self::Int(IntNode::new(value))
    }
}

impl From<f64> for ExpressionNode {
    fn from(value: f64) -> Self {
        Self::Float(FloatNode::new(value))
    }
}

impl From<&str> for ExpressionNode {
    fn from(value: &str) -> Self {
        Self::String(StringNode {
            value: String::from(value),
            span: None,
        })
    }
}

impl From<Vec<ExpressionNode>> for ExpressionNode {
    fn from(value: Vec<ExpressionNode>) -> Self {
        Self::Array(ArrayNode { value, span: None })
    }
}

impl From<Vec<&str>> for ExpressionNode {
    fn from(vec: Vec<&str>) -> Self {
        let value = vec
            .into_iter()
            .map(|i| ExpressionNode::from(i))
            .collect::<Vec<_>>();
        Self::Array(ArrayNode { value, span: None })
    }
}

impl From<Vec<LpcInt>> for ExpressionNode {
    fn from(vec: Vec<LpcInt>) -> Self {
        let value = vec
            .into_iter()
            .map(|i| ExpressionNode::from(i))
            .collect::<Vec<_>>();
        Self::Array(ArrayNode { value, span: None })
    }
}

impl From<HashMap<ExpressionNode, ExpressionNode>> for ExpressionNode {
    fn from(map: HashMap<ExpressionNode, ExpressionNode>) -> Self {
        let value = map.into_iter().collect::<Vec<_>>();

        Self::Mapping(MappingNode { value, span: None })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{assignment_node::AssignmentOperation, binary_op_node::BinaryOperation},
        LpcFloat,
    };

    use super::*;

    #[test]
    fn test_from_binary_op_node() {
        let node = BinaryOpNode {
            l: Box::new(IntNode::new(666).into()),
            r: Box::new(IntNode::new(324).into()),
            op: BinaryOperation::Add,
            span: None,
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
            span: None,
        };

        let clone = node.clone();

        assert_eq!(
            ExpressionNode::from(node),
            ExpressionNode::Assignment(clone)
        );
    }

    #[test]
    fn test_from_call_node() {
        let node = CallNode {
            arguments: vec![],
            name: "foo".to_string(),
            span: None,
        };

        let clone = node.clone();

        assert_eq!(ExpressionNode::from(node), ExpressionNode::Call(clone));
    }

    #[test]
    fn test_from_range_node() {
        let node = RangeNode {
            l: Box::new(Some(ExpressionNode::from(666))),
            r: Box::new(Some(ExpressionNode::from(432))),
            span: None,
        };

        let clone = node.clone();

        assert_eq!(ExpressionNode::from(node), ExpressionNode::Range(clone));
    }

    mod from_ast_node {
        use crate::ast::{
            ast_node::AstNode, expression_node::ExpressionNode, int_node::IntNode,
            program_node::ProgramNode,
        };

        #[test]
        fn test_from_ast_node_is_ok_for_expressions() {
            let expression_node = ExpressionNode::Int(IntNode::new(666));
            let clone = expression_node.clone();
            let ast_node = AstNode::Expression(expression_node);

            assert_eq!(ExpressionNode::from(ast_node), clone);
        }

        #[test]
        #[should_panic]
        fn test_from_ast_node_is_panics_for_non_expressions() {
            let node = ProgramNode::default();
            let ast_node = AstNode::Program(node);

            ExpressionNode::from(ast_node);
        }
    }

    #[test]
    fn test_from_int() {
        let i = 666;

        assert_eq!(
            ExpressionNode::from(i),
            ExpressionNode::Int(IntNode {
                value: i,
                span: None
            })
        );
    }

    #[test]
    fn test_from_float() {
        let f = 666.69;

        assert_eq!(
            ExpressionNode::from(f),
            ExpressionNode::Float(FloatNode {
                value: LpcFloat::from(f),
                span: None
            })
        );
    }

    #[test]
    fn test_from_str() {
        let s = "hello";

        assert_eq!(
            ExpressionNode::from(s),
            ExpressionNode::String(StringNode {
                value: String::from(s),
                span: None
            })
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
            ExpressionNode::Array(ArrayNode {
                value: vec,
                span: None
            })
        );
    }

    #[test]
    fn test_from_str_vec() {
        let vec = vec!["foo", "asdfas", "asdkj", "as"];

        let value = vec
            .iter()
            .map(|i| ExpressionNode::from(*i))
            .collect::<Vec<_>>();
        assert_eq!(
            ExpressionNode::from(vec.to_vec()),
            ExpressionNode::Array(ArrayNode { value, span: None })
        );
    }

    #[test]
    fn test_from_int_vec() {
        let vec = vec![123, 31, -4567, 8238];

        let value = vec
            .iter()
            .map(|i| ExpressionNode::from(*i))
            .collect::<Vec<_>>();
        assert_eq!(
            ExpressionNode::from(vec.to_vec()),
            ExpressionNode::Array(ArrayNode { value, span: None })
        );
    }
}
