use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::call_node::CallNode;
use crate::ast::ast_node::ASTNodeTrait;

pub trait TreeWalker {
    fn walk_tree(&mut self, root: &impl ASTNodeTrait);

    fn visit_program(&mut self, program: &ProgramNode);
    fn visit_call(&mut self, program: &CallNode);
    fn visit_int(&mut self, program: &IntNode);
    fn visit_binary_op(&mut self, program: &BinaryOpNode);
}
