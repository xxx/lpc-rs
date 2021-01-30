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
use crate::errors::CompilerError;

/// A tree walker for pretty-printing an AST
///
/// # Examples
/// ```
/// use mathstack::mathstack_parser;
/// use mathstack::codegen::tree_printer::TreePrinter;
/// use mathstack::codegen::tree_walker::TreeWalker;
/// let prog = "int main() { int b = 123; return b; }";
/// let program_node = mathstack_parser::ProgramParser::new().parse(prog).unwrap();
/// let mut walker = TreePrinter::new();
/// walker.visit_program(&program_node);
/// ```
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
    fn visit_program(&mut self, program: &ProgramNode) -> Result<(), CompilerError> {
        println!("Program");
        self.indent += 2;
        for expr in &program.functions {
            expr.visit(self);
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_call(&mut self, node: &CallNode) -> Result<(), CompilerError> {
        self.println_indented("Call");
        self.indent += 2;
        self.println_indented(&format!("id: {}", node.name));
        self.println_indented("args:");
        self.indent += 2;
        for arg in &node.arguments {
            arg.visit(self);
        }
        self.indent -= 4;

        Ok(())
    }

    fn visit_int(&mut self, int: &IntNode) -> Result<(), CompilerError> {
        self.println_indented(&format!("Int: {}", int.value));

        Ok(())
    }

    fn visit_binary_op(&mut self, expression: &BinaryOpNode) -> Result<(), CompilerError> {
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

        Ok(())
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) -> Result<(), CompilerError> {
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

        Ok(())
    }

    fn visit_decl(&mut self, node: &DeclNode) -> Result<(), CompilerError> {
        self.println_indented("Decl");
        self.indent += 2;
        self.println_indented(&format!("type: {}", node.type_));
        self.println_indented("initializations:");
        self.indent += 2;
        for init in &node.initializations {
            init.visit(self);
        }
        self.indent -= 4;

        Ok(())
    }

    fn visit_var_init(&mut self, node: &VarInitNode) -> Result<(), CompilerError> {
        self.println_indented("VarInit");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("type: {}", node.type_));
        self.println_indented(&format!("value: {:?}", node.value));
        self.println_indented(&format!("array: {:?}", node.array));

        Ok(())
    }
}
