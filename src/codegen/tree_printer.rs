use tree_walker::TreeWalker;

use crate::{
    ast::{
        array_node::ArrayNode, ast_node::AstNodeTrait, binary_op_node::BinaryOpNode,
        block_node::BlockNode, call_node::CallNode, comma_expression_node::CommaExpressionNode,
        decl_node::DeclNode, float_node::FloatNode, function_def_node::FunctionDefNode,
        int_node::IntNode, mapping_node::MappingNode, program_node::ProgramNode,
        range_node::RangeNode, return_node::ReturnNode, string_node::StringNode,
        var_init_node::VarInitNode, var_node::VarNode,
    },
    codegen::tree_walker,
    errors::LpcError,
};

/// A tree walker for pretty-printing an AST
///
/// # Examples
/// ```
/// use lpc_rs::lpc_parser;
/// use lpc_rs::codegen::tree_printer::TreePrinter;
/// use lpc_rs::codegen::tree_walker::TreeWalker;
/// use lpc_rs::parser::lexer::LexWrapper;
///
/// let prog = "int main() { int b = 123; return b; }";
/// let lexer = LexWrapper::new(prog);
/// let mut program_node = lpc_parser::ProgramParser::new().parse(lexer).unwrap();
/// let mut walker = TreePrinter::new();
/// walker.visit_program(&mut program_node);
/// ```
#[derive(Debug)]
pub struct TreePrinter {
    indent: usize,
}

impl TreePrinter {
    pub fn new() -> Self {
        Self { indent: 0 }
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
    fn visit_program(&mut self, program: &mut ProgramNode) -> Result<(), LpcError> {
        println!("Program");
        self.indent += 2;
        for expr in &mut program.body {
            expr.visit(self)?;
        }
        self.indent -= 2;

        Ok(())
    }

    /// Visit a code block
    fn visit_block(&mut self, node: &mut BlockNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        self.println_indented("Block {");

        self.indent += 2;

        for expr in &mut node.body {
            expr.visit(self)?;
        }

        self.indent -= 2;

        self.println_indented("}");

        Ok(())
    }

    fn visit_call(&mut self, node: &mut CallNode) -> Result<(), LpcError> {
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

    fn visit_int(&mut self, node: &mut IntNode) -> Result<(), LpcError> {
        self.println_indented(&format!("Int: {}", node.value));

        Ok(())
    }

    fn visit_float(&mut self, node: &mut FloatNode) -> Result<(), LpcError> {
        self.println_indented(&format!("Float: {}", node.value));

        Ok(())
    }

    fn visit_string(&mut self, node: &mut StringNode) -> Result<(), LpcError> {
        self.println_indented(&format!("String: {}", node.value));

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<(), LpcError> {
        self.println_indented("Binary Op");
        self.indent += 2;
        self.println_indented(&format!("operation: {:?}", node.op));
        self.println_indented("l: ");
        self.indent += 2;
        node.l.visit(self)?;
        self.indent -= 2;
        self.println_indented("r: ");
        self.indent += 2;
        node.r.visit(self)?;
        self.indent -= 4;

        Ok(())
    }

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<(), LpcError> {
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

    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<(), LpcError> {
        self.println_indented("Return");
        self.indent += 2;
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_decl(&mut self, node: &mut DeclNode) -> Result<(), LpcError> {
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

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<(), LpcError> {
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

    fn visit_var(&mut self, node: &mut VarNode) -> Result<(), LpcError> {
        self.println_indented("Var");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.indent -= 2;

        Ok(())
    }

    fn visit_array(&mut self, node: &mut ArrayNode) -> Result<(), LpcError> {
        self.println_indented("Array ({");
        self.indent += 2;
        for node in &mut node.value {
            node.visit(self)?;
        }
        self.indent -= 2;
        self.println_indented("})");

        Ok(())
    }

    fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<(), LpcError> {
        self.println_indented("Mapping ([");
        self.indent += 2;
        for (key, value) in &mut node.value {
            key.visit(self)?;
            self.println_indented(": ");
            value.visit(self)?;
        }
        self.indent -= 2;
        self.println_indented("])");

        Ok(())
    }

    fn visit_range(&mut self, node: &mut RangeNode) -> Result<(), LpcError> {
        self.println_indented("Range");

        self.println_indented("l: ");
        self.indent += 2;
        if let Some(expr) = &mut *node.l {
            expr.visit(self)?;
        } else {
            self.println_indented("None");
        }
        self.indent -= 2;

        self.println_indented("r: ");
        self.indent += 2;
        if let Some(expr) = &mut *node.r {
            expr.visit(self)?;
        } else {
            self.println_indented("None");
        }

        self.indent -= 2;

        Ok(())
    }

    fn visit_comma_expression(&mut self, node: &mut CommaExpressionNode) -> Result<(), LpcError> {
        self.println_indented("Comma Expression");
        self.indent += 2;
        for expr in &mut node.value {
            let _ = expr.visit(self);
        }
        self.indent -= 2;

        Ok(())
    }
}
