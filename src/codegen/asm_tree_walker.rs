use crate::ast::program_node::ProgramNode;
use crate::ast::expression_node::ExpressionNode;
use crate::ast::int_node::IntNode;
use crate::codegen::tree_walker::TreeWalkerTrait;
use crate::ast::ast_node::ASTNodeTrait;

#[derive(Debug)]
pub struct AsmTreeWalker;

impl TreeWalkerTrait for AsmTreeWalker {
    fn walk_tree(&self, root: &impl ASTNodeTrait) {
        root.visit(self);
    }

    fn visit_program(&self, program: &ProgramNode) {
        for expr in &program.expressions {
            self.walk_tree(&expr);
        }
    }

    fn visit_int(&self, int: &IntNode) {
        println!("int node: {}", int.value);
    }

    fn visit_expression(&self, expression: &ExpressionNode) {
        println!("Expression node");
        print!("  l:");
        self.walk_tree(&(*expression.l));
        print!("  r:");
        self.walk_tree(&(*expression.r));
        println!("  operation: {:?}", expression.op);
    }
}
