use crate::codegen::tree_walker;
use tree_walker::TreeWalker;
use crate::ast::int_node::IntNode;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::call_node::CallNode;

#[derive(Debug, Default)]
pub struct LocalCounterWalker {
    pub count: usize
}

impl TreeWalker for LocalCounterWalker {
    fn visit_call(&mut self, node: &CallNode) {
        self.count += node.arguments.len();
        for argument in &node.arguments {
            argument.visit(self);
        }
    }

    fn visit_int(&mut self, _node: &IntNode) {
        self.count += 1;
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) {
        node.l.visit(self);
        node.r.visit(self);
        self.count += 1; // store result
    }
}
