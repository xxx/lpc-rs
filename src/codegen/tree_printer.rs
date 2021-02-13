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
use crate::errors::compiler_error::CompilerError;
use crate::ast::array_node::ArrayNode;
use crate::ast::return_node::ReturnNode;
use crate::ast::var_node::VarNode;

/// A tree walker for pretty-printing an AST
///
/// # Examples
/// ```
/// use std::borrow::BorrowMut;
/// use lpc_rs::lpc_parser;
/// use lpc_rs::codegen::tree_printer::TreePrinter;
/// use lpc_rs::codegen::tree_walker::TreeWalker;
/// let prog = "int main() { int b = 123; return b; }";
/// let mut program_node = lpc_parser::ProgramParser::new().parse(prog).unwrap();
/// let mut walker = TreePrinter::new();
/// walker.visit_program(program_node.borrow_mut());
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
    fn visit_program(&mut self, program: &mut ProgramNode) -> Result<(), CompilerError> {
        println!("Program");
        self.indent += 2;
        for expr in &mut program.body {
            expr.visit(self)?;
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_call(&mut self, node: &mut CallNode) -> Result<(), CompilerError> {
        self.println_indented("Call");
        self.indent += 2;
        self.println_indented(&format!("id: {}", node.name));
        self.println_indented("args:");
        self.indent += 2;
        for arg in &mut node.arguments {
            arg.visit(self)?;
        }
        self.indent -= 4;

        Ok(())
    }

    fn visit_int(&mut self, int: &mut IntNode) -> Result<(), CompilerError> {
        self.println_indented(&format!("Int: {}", int.value));

        Ok(())
    }

    fn visit_binary_op(&mut self, expression: &mut BinaryOpNode) -> Result<(), CompilerError> {
        self.println_indented("Binary Op");
        self.indent += 2;
        self.println_indented(&format!("operation: {:?}", expression.op));
        self.println_indented("l: ");
        self.indent += 2;
        expression.l.visit(self)?;
        self.indent -= 2;
        self.println_indented("r: ");
        self.indent += 2;
        expression.r.visit(self)?;
        self.indent -= 4;

        Ok(())
    }

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<(), CompilerError> {
        self.println_indented("Function Def");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("return type: {}", node.return_type));
        self.println_indented("parameters:");
        self.indent += 2;
        for parameter in &mut node.parameters {
            parameter.visit(self)?;
        }
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        for expression in &mut node.body {
            expression.visit(self)?;
        }
        self.indent -= 4;

        Ok(())
    }

    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<(), CompilerError> {
        self.println_indented("Return");
        self.indent += 2;
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_decl(&mut self, node: &mut DeclNode) -> Result<(), CompilerError> {
        self.println_indented("Decl");
        self.indent += 2;
        self.println_indented(&format!("type: {}", node.type_));
        self.println_indented("initializations:");
        self.indent += 2;
        for init in &mut node.initializations {
            init.visit(self)?;
        }
        self.indent -= 4;

        Ok(())
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<(), CompilerError> {
        self.println_indented("VarInit");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("type: {}", node.type_));
        if let Some(node) = &mut node.value {
            self.println_indented("value:");
            self.indent += 2;
            node.visit(self)?;
            self.indent -= 2;
        } else {
            self.println_indented("value: None");
        }
        self.println_indented(&format!("array: {}", node.array));
        self.indent -= 2;

        Ok(())
    }

    fn visit_var(&mut self, node: &mut VarNode) -> Result<(), CompilerError> {
        self.println_indented("Var");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.indent -= 2;

        Ok(())
    }

    fn visit_array(&mut self, node: &mut ArrayNode) -> Result<(), CompilerError> {
        self.println_indented("Array ({");
        self.indent += 2;
        for node in &mut node.value {
            node.visit(self)?;
        }
        self.indent -= 2;
        self.println_indented("})");

        Ok(())
    }
}
