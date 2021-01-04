use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::codegen::tree_walker::TreeWalkerTrait;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::binary_op_node::BinaryOpNode;

#[derive(Debug)]
pub struct TreePrinter {
    indent: usize
}

impl TreePrinter {
    pub fn new() -> Self {
        Self {
            indent: 0
        }
    }

    fn println_indented(&self, output: String) {
        println!("{:width$}{}", "", output, width = self.indent);
    }
}

impl TreeWalkerTrait for TreePrinter {
    fn walk_tree(&mut self, root: &impl ASTNodeTrait) {
        root.visit(self);
    }

    fn visit_program(&mut self, program: &ProgramNode) {
        println!("Program node");
        self.indent += 2;
        for expr in &program.expressions {
            self.walk_tree(&expr);
        }
        self.indent -= 2;
    }

    fn visit_int(&mut self, int: &IntNode) {
        self.println_indented(format!("Int Node: {}", int.value));
    }

    fn visit_binary_op(&mut self, expression: &BinaryOpNode) {
        self.println_indented(String::from("Binary Op Node"));
        self.println_indented(format!("operation: {:?}", expression.op));
        self.println_indented(String::from("l: "));
        self.indent += 2;
        self.walk_tree(&(*expression.l));
        self.indent -= 2;
        self.println_indented(String::from("r: "));
        self.indent += 2;
        self.walk_tree(&(*expression.r));
        self.indent -= 2;
    }
}
