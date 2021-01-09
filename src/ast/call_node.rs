use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::expression_node::ExpressionNode;
use crate::codegen::tree_walker::TreeWalker;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CallNode {
    pub arguments: Vec<ExpressionNode>,
    pub id: String
}

impl ASTNodeTrait for CallNode {
    fn to_str(&self) -> String {
        format!("CallNode[{:?}]", self)
    }

    fn visit(&self, tree_walker: &mut impl TreeWalker) {
        tree_walker.visit_call(self);
    }
}