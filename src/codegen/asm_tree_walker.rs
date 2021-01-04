use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::codegen::tree_walker::TreeWalkerTrait;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::binary_op_node::BinaryOpNode;

#[derive(Debug)]
pub struct AsmTreeWalker;

impl TreeWalkerTrait for AsmTreeWalker {
    fn walk_tree(&mut self, root: &impl ASTNodeTrait) {
        root.visit(self);
    }

    fn visit_program(&mut self, program: &ProgramNode) {
        for expr in &program.expressions {
            self.walk_tree(&expr);
        }
    }

    fn visit_int(&mut self, int: &IntNode) {
        println!("int node: {}", int.value);
    }

    fn visit_binary_op(&mut self, expression: &BinaryOpNode) {
        println!("Binary op node");
        print!("  l:");
        self.walk_tree(&(*expression.l));
        print!("  r:");
        self.walk_tree(&(*expression.r));
        println!("  operation: {:?}", expression.op);
    }
}
