use crate::codegen::asm_tree_walker::AsmTreeWalker;
use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::binary_op_node::BinaryOpNode;

pub trait TreeWalker {
    fn walk_tree(&mut self, root: &impl ASTNodeTrait);

    fn visit_program(&mut self, program: &ProgramNode);
    fn visit_int(&mut self, program: &IntNode);
    fn visit_binary_op(&mut self, program: &BinaryOpNode);
}
