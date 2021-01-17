use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::call_node::CallNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::function_def_node::FunctionDefNode;

pub fn walk_tree(root: &impl ASTNodeTrait, walker: &mut impl TreeWalker) {
    root.visit(walker);
}

pub trait TreeWalker {
    fn visit_program(&mut self, node: &ProgramNode);
    fn visit_call(&mut self, node: &CallNode);
    fn visit_int(&mut self, node: &IntNode);
    fn visit_binary_op(&mut self, node: &BinaryOpNode);
    fn visit_function_def(&mut self, _node: &FunctionDefNode) {}
}
