use std::{
    fmt,
    fmt::{Display, Formatter},
};

use indexmap::IndexMap;
use lpc_rs_core::{BaseFloat, LpcInt};
use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::{
        array_node::ArrayNode,
        assignment_node::AssignmentNode,
        ast_node::{AstNodeTrait, SpannedNode},
        binary_op_node::BinaryOpNode,
        call_node::CallNode,
        closure_node::ClosureNode,
        comma_expression_node::CommaExpressionNode,
        float_node::FloatNode,
        function_ptr_node::FunctionPtrNode,
        int_node::IntNode,
        mapping_node::MappingNode,
        range_node::RangeNode,
        string_node::StringNode,
        ternary_node::TernaryNode,
        unary_op_node::UnaryOpNode,
        var_node::VarNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A wrapper node for anything that can be considered an expression
/// (i.e. an operation that returns a value)
#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub enum ExpressionNode {
    Assignment(AssignmentNode),
    BinaryOp(BinaryOpNode),
    Call(CallNode),
    Closure(ClosureNode),
    CommaExpression(CommaExpressionNode),
    Float(FloatNode),
    FunctionPtr(FunctionPtrNode),
    Int(IntNode),
    Range(RangeNode),
    String(StringNode),
    Ternary(TernaryNode),
    UnaryOp(UnaryOpNode),
    Var(VarNode),
    Array(ArrayNode),
    Mapping(MappingNode),
}

/// A convenience helper to get the first `span` we can find in a list of nodes.
/// Returns a default if no spans are found.
///
/// # Arguments
/// `nodes` - A reference to a slice of Expression nodes.
pub fn first_span(nodes: &[&ExpressionNode]) -> Span {
    nodes
        .iter()
        .find(|node| node.span().is_some())
        .and_then(|node| node.span())
        .unwrap_or_else(|| Span::new(0, 0..0))
}

macro_rules! delegated_traits {
    ( $( $x:path ),+ ) => {
        impl AstNodeTrait for ExpressionNode {
            fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
                match self {
                $(
                    $x(y) => y.visit(tree_walker),
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

        impl SpannedNode for ExpressionNode {
            fn span(&self) -> Option<Span> {
                match self {
                    $(
                        $x(y) => y.span,
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
    ExpressionNode::Closure,
    ExpressionNode::CommaExpression,
    ExpressionNode::Float,
    ExpressionNode::FunctionPtr,
    ExpressionNode::Int,
    ExpressionNode::Range,
    ExpressionNode::String,
    ExpressionNode::Ternary,
    ExpressionNode::UnaryOp,
    ExpressionNode::Var,
    ExpressionNode::Array,
    ExpressionNode::Mapping
);

impl From<BinaryOpNode> for ExpressionNode {
    fn from(node: BinaryOpNode) -> Self {
        Self::BinaryOp(node)
    }
}

impl From<UnaryOpNode> for ExpressionNode {
    fn from(node: UnaryOpNode) -> Self {
        Self::UnaryOp(node)
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

impl From<TernaryNode> for ExpressionNode {
    fn from(node: TernaryNode) -> Self {
        Self::Ternary(node)
    }
}

impl From<FunctionPtrNode> for ExpressionNode {
    fn from(node: FunctionPtrNode) -> Self {
        Self::FunctionPtr(node)
    }
}

impl From<LpcInt> for ExpressionNode {
    fn from(value: LpcInt) -> Self {
        Self::Int(IntNode::new(value))
    }
}

impl From<BaseFloat> for ExpressionNode {
    fn from(value: BaseFloat) -> Self {
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
            .map(ExpressionNode::from)
            .collect::<Vec<_>>();
        Self::Array(ArrayNode { value, span: None })
    }
}

impl From<Vec<LpcInt>> for ExpressionNode {
    fn from(vec: Vec<LpcInt>) -> Self {
        let value = vec
            .into_iter()
            .map(ExpressionNode::from)
            .collect::<Vec<_>>();
        Self::Array(ArrayNode { value, span: None })
    }
}

impl From<IndexMap<ExpressionNode, ExpressionNode>> for ExpressionNode {
    fn from(map: IndexMap<ExpressionNode, ExpressionNode>) -> Self {
        let value = map.into_iter().collect::<Vec<_>>();

        Self::Mapping(MappingNode { value, span: None })
    }
}

#[cfg(test)]
mod tests {
    use factori::create;
    use lpc_rs_core::LpcFloat;

    use super::*;
    use crate::{compiler::ast::binary_op_node::BinaryOperation, test_support::factories::*};

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

        let clone = node;

        assert_eq!(ExpressionNode::from(node), ExpressionNode::Int(clone));
    }

    #[test]
    fn test_from_assignment_node() {
        let node = AssignmentNode {
            lhs: Box::new(VarNode::new("adsf").into()),
            rhs: Box::new(IntNode::new(324).into()),
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
        let node = create!(CallNode);

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
