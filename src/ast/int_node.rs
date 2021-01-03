use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalkerTrait;

#[derive(Debug, Copy, Clone)]
pub struct IntNode {
    pub value: i64,
}

impl ASTNodeTrait for IntNode {
    fn to_str(&self) -> String {
        format!("IntNode[{}]", self.value)
    }

    fn visit(&self, tree_walker: &mut impl TreeWalkerTrait) { tree_walker.visit_int(self); }
}