use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;

#[derive(Debug, Copy, Clone)]
pub struct IntNode {
    pub value: i64,
}

impl IntNode {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

impl ASTNodeTrait for IntNode {
    fn to_str(&self) -> String {
        format!("IntNode[{}]", self.value)
    }

    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_int(self); }
}