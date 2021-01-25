use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::codegen::tree_walker;
use tree_walker::TreeWalker;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::call_node::CallNode;
use crate::ast::function_def_node::FunctionDefNode;
use crate::ast::decl_node::DeclNode;
use crate::ast::var_init_node::VarInitNode;

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

    fn println_indented(&self, output: &str) {
        println!("{:width$}{}", "", output, width = self.indent);
    }
}

impl Default for TreePrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl TreeWalker for TreePrinter {
    fn visit_program(&mut self, program: &ProgramNode) {
        println!("Program");
        self.indent += 2;
        for expr in &program.functions {
            expr.visit(self);
        }
        self.indent -= 2;
    }

    fn visit_call(&mut self, node: &CallNode) {
        self.println_indented("Call");
        self.indent += 2;
        self.println_indented(&format!("id: {}", node.id));
        self.println_indented("args:");
        self.indent += 2;
        for arg in &node.arguments {
            arg.visit(self);
        }
        self.indent -= 4;
    }

    fn visit_int(&mut self, int: &IntNode) {
        self.println_indented(&format!("Int: {}", int.value));
    }

    fn visit_binary_op(&mut self, expression: &BinaryOpNode) {
        self.println_indented("Binary Op");
        self.indent += 2;
        self.println_indented(&format!("operation: {:?}", expression.op));
        self.println_indented("l: ");
        self.indent += 2;
        expression.l.visit(self);
        self.indent -= 2;
        self.println_indented("r: ");
        self.indent += 2;
        expression.r.visit(self);
        self.indent -= 4;
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) {
        self.println_indented("Function Def");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("return type: {}", node.return_type));
        self.println_indented("parameters:");
        self.indent += 2;
        for parameter in &node.parameters {
            parameter.visit(self);
        }
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        for expression in &node.body {
            expression.visit(self);
        }
        self.indent -= 4;
    }

    fn visit_decl(&mut self, node: &DeclNode) {
        self.println_indented("Decl");
        self.indent += 2;
        self.println_indented(&format!("type: {}", node.type_));
        self.println_indented("initializations:");
        self.indent += 2;
        for init in &node.initializations {
            init.visit(self);
        }
        self.indent -= 4;
    }

    fn visit_var_init(&mut self, node: &VarInitNode) {
        self.println_indented("VarInit");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("type: {}", node.type_));
        self.println_indented(&format!("value: {:?}", node.value));
        self.println_indented(&format!("array: {:?}", node.array));
    }
}
