use tree_walker::TreeWalker;

use crate::{
    ast::{
        array_node::ArrayNode, ast_node::AstNodeTrait, binary_op_node::BinaryOpNode,
        block_node::BlockNode, call_node::CallNode, comma_expression_node::CommaExpressionNode,
        decl_node::DeclNode, do_while_node::DoWhileNode, float_node::FloatNode,
        function_def_node::FunctionDefNode, function_ptr_node::FunctionPtrNode, if_node::IfNode,
        inherit_node::InheritNode, int_node::IntNode, mapping_node::MappingNode,
        program_node::ProgramNode, range_node::RangeNode, return_node::ReturnNode,
        string_node::StringNode, unary_op_node::UnaryOpNode, var_init_node::VarInitNode,
        var_node::VarNode, while_node::WhileNode,
    },
    codegen::tree_walker,
    Result,
};
use crate::ast::for_each_node::ForEachNode;
use crate::ast::for_node::ForNode;

/// A tree walker for pretty-printing an AST
///
/// # Examples
/// ```
/// use lpc_rs::lpc_parser;
/// use lpc_rs::codegen::tree_printer::TreePrinter;
/// use lpc_rs::codegen::tree_walker::TreeWalker;
/// use lpc_rs::parser::lexer::LexWrapper;
/// use lpc_rs::compilation_context::CompilationContext;
///
/// let prog = "int main() { int b = 123; return b; }";
/// let lexer = LexWrapper::new(prog);
/// let mut program_node = lpc_parser::ProgramParser::new().parse(&CompilationContext::default(), lexer).unwrap();
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
    fn visit_array(&mut self, node: &mut ArrayNode) -> Result<()> {
        self.println_indented("Array ({");
        self.indent += 2;
        for node in &mut node.value {
            node.visit(self)?;
        }
        self.indent -= 2;
        self.println_indented("})");

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()> {
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

    fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        self.println_indented("Block {");

        self.indent += 2;

        for expr in &mut node.body {
            expr.visit(self)?;
        }

        self.indent -= 2;

        self.println_indented("}");

        Ok(())
    }

    fn visit_call(&mut self, node: &mut CallNode) -> Result<()> {
        if let Some(rcvr) = &node.receiver {
            self.println_indented("Call Other");
            self.indent += 2;
            self.println_indented(&format!("receiver: {}", rcvr));
        } else {
            self.println_indented("Call");
            self.indent += 2;
        }
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented("args:");
        self.indent += 2;
        for arg in &mut node.arguments {
            arg.visit(self)?;
        }
        self.indent -= 4;

        Ok(())
    }

    fn visit_comma_expression(&mut self, node: &mut CommaExpressionNode) -> Result<()> {
        self.println_indented("Comma Expression");
        self.indent += 2;
        for expr in &mut node.value {
            let _ = expr.visit(self);
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()> {
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

    fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        self.println_indented("Do");
        self.indent += 2;
        let _ = node.body.visit(self);
        self.indent -= 2;
        self.println_indented("while:");
        self.indent += 2;
        let _ = node.condition.visit(self);
        self.indent -= 2;

        Ok(())
    }

    fn visit_float(&mut self, node: &mut FloatNode) -> Result<()> {
        self.println_indented(&format!("Float: {}", node.value));

        Ok(())
    }

    fn visit_for(&mut self, node: &mut ForNode) -> Result<()>
        where
            Self: Sized,
    {
        self.println_indented("For:");
        self.indent += 2;
        self.println_indented("init:");
        self.indent += 2;
        if let Some(init) = &mut *node.initializer {
            init.visit(self)?;
        }
        self.indent -= 2;
        self.println_indented("condition:");
        self.indent += 2;
        if let Some(cond) = &mut node.condition {
            cond.visit(self)?;
        }
        self.indent -= 2;
        self.println_indented("incrementer:");
        self.indent += 2;
        if let Some(incr) = &mut node.incrementer {
            incr.visit(self)?;
        }

        self.indent -= 2;

        self.println_indented("body:");

        self.indent += 2;

        node.body.visit(self)?;

        self.indent -= 4;

        Ok(())
    }

    fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()>
        where
            Self: Sized,
    {
        self.println_indented("Foreach:");
        self.indent += 2;
        self.println_indented(&format!("init: {}", node.initializer));
        self.println_indented("collection:");
        node.collection.visit(self)?;

        self.indent += 2;
        node.body.visit(self)?;

        self.indent -= 4;

        Ok(())
    }

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
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

    fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()> {
        self.println_indented("Function Ptr");
        self.indent += 2;

        if let Some(rcvr) = &mut node.receiver {
            self.println_indented(&format!("receiver: {}", rcvr));
        } else {
            self.println_indented("receiver: None");
        }

        self.println_indented(&format!("name: {}", node.name));

        self.println_indented("arguments:");
        self.indent += 2;

        if let Some(args) = &mut node.arguments {
            for argument in args {
                if let Some(n) = argument {
                    n.visit(self)?;
                } else {
                    self.println_indented("None");
                }
            }
        } else {
            self.println_indented("None");
        }

        self.indent -= 4;

        Ok(())
    }

    fn visit_if(&mut self, node: &mut IfNode) -> Result<()> {
        self.println_indented("If");
        self.indent += 2;
        let _ = node.condition.visit(self);
        self.indent -= 2;
        self.println_indented("then");
        self.indent += 2;
        let _ = node.body.visit(self);
        self.indent -= 2;

        if let Some(n) = &mut *node.else_clause {
            self.println_indented("else");
            self.indent += 2;

            let _ = n.visit(self);
            self.indent -= 2;
        }

        Ok(())
    }

    fn visit_inherit(&mut self, node: &mut InheritNode) -> Result<()> {
        self.println_indented("Inherit:");

        self.indent += 2;

        self.println_indented(&node.path);

        if let Some(namespace) = &node.namespace {
            self.println_indented(namespace);
        }

        self.indent -= 2;

        Ok(())
    }

    fn visit_int(&mut self, node: &mut IntNode) -> Result<()> {
        self.println_indented(&format!("Int: {}", node.value));

        Ok(())
    }

    fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()> {
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

    fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()> {
        println!("Program");
        self.indent += 2;
        for expr in &mut node.inherits {
            expr.visit(self)?;
        }

        for expr in &mut node.body {
            expr.visit(self)?;
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_range(&mut self, node: &mut RangeNode) -> Result<()> {
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

    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()> {
        self.println_indented("Return");
        self.indent += 2;
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_string(&mut self, node: &mut StringNode) -> Result<()> {
        self.println_indented(&format!("String: {}", node.value));

        Ok(())
    }

    fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()> {
        self.println_indented("Unary Op");
        self.indent += 2;
        self.println_indented(&format!("operation: {:?}", node.op));
        self.println_indented("expr: ");
        self.indent += 2;
        node.expr.visit(self)?;
        self.indent -= 4;

        Ok(())
    }

    fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        self.println_indented("Var");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.indent -= 2;

        Ok(())
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
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

    fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        self.println_indented("While");
        self.indent += 2;
        let _ = node.condition.visit(self);
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        let _ = node.body.visit(self);
        self.indent -= 2;

        Ok(())
    }
}
