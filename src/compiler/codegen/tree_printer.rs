use lpc_rs_errors::Result;
use qcell::QCellOwner;
use tree_walker::TreeWalker;

use crate::compiler::{
    ast::{
        array_node::ArrayNode, ast_node::AstNodeTrait, binary_op_node::BinaryOpNode,
        block_node::BlockNode, call_node::CallNode, comma_expression_node::CommaExpressionNode,
        decl_node::DeclNode, do_while_node::DoWhileNode, float_node::FloatNode,
        for_each_node::ForEachNode, for_node::ForNode, function_def_node::FunctionDefNode,
        function_ptr_node::FunctionPtrNode, if_node::IfNode, inherit_node::InheritNode,
        int_node::IntNode, mapping_node::MappingNode, program_node::ProgramNode,
        range_node::RangeNode, return_node::ReturnNode, string_node::StringNode,
        unary_op_node::UnaryOpNode, var_init_node::VarInitNode, var_node::VarNode,
        while_node::WhileNode,
    },
    codegen::tree_walker,
};
use crate::compiler::ast::call_node::CallChain;

/// A tree walker for pretty-printing an AST
///
/// # Examples
/// ```
/// use lpc_rs::{
///     compiler::{
///         codegen::{tree_printer::TreePrinter, tree_walker::TreeWalker},
///         compilation_context::CompilationContext,
///         lexer::LexWrapper,
///     },
///     lpc_parser,
/// };
/// use qcell::QCellOwner;
///
/// let prog = "int main() { int b = 123; return b; }";
/// let lexer = LexWrapper::new(prog);
/// let mut cell_key = QCellOwner::new();
/// let mut program_node = lpc_parser::ProgramParser::new()
///     .parse(&mut CompilationContext::default(), lexer)
///     .unwrap();
/// let mut walker = TreePrinter::new();
/// walker.visit_program(&mut program_node, &mut cell_key);
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
    fn visit_array(&mut self, node: &mut ArrayNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Array ({");
        self.indent += 2;
        for node in &mut node.value {
            node.visit(self, cell_key)?;
        }
        self.indent -= 2;
        self.println_indented("})");

        Ok(())
    }

    fn visit_binary_op(
        &mut self,
        node: &mut BinaryOpNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        self.println_indented("Binary Op");
        self.indent += 2;
        self.println_indented(&format!("operation: {:?}", node.op));
        self.println_indented("l: ");
        self.indent += 2;
        node.l.visit(self, cell_key)?;
        self.indent -= 2;
        self.println_indented("r: ");
        self.indent += 2;
        node.r.visit(self, cell_key)?;
        self.indent -= 4;

        Ok(())
    }

    fn visit_block(&mut self, node: &mut BlockNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Block {");

        self.indent += 2;

        for expr in &mut node.body {
            expr.visit(self, cell_key)?;
        }

        self.indent -= 2;

        self.println_indented("}");

        Ok(())
    }

    fn visit_call(&mut self, node: &mut CallNode, cell_key: &mut QCellOwner) -> Result<()> {
        match &mut node.chain {
            CallChain::Root { receiver, name, .. } => {
                if let Some(rcvr) = receiver {
                    self.println_indented("Call Other");
                    self.indent += 2;
                    self.println_indented(&format!("receiver: {rcvr}"));
                } else {
                    self.println_indented("Call");
                    self.indent += 2;
                }
                self.println_indented(&format!("name: {}", name));
                self.println_indented("args:");
                self.indent += 2;
                for arg in &mut node.arguments {
                    arg.visit(self, cell_key)?;
                }
                self.indent -= 4;
            }
            CallChain::Node(ref mut chain_node) => {
                chain_node.visit(self, cell_key)?;
                self.indent += 2;
                self.println_indented("chain args:");
                self.indent += 2;
                for arg in &mut node.arguments {
                    arg.visit(self, cell_key)?;
                }
                self.indent -= 4;
            }
        }

        Ok(())
    }

    fn visit_comma_expression(
        &mut self,
        node: &mut CommaExpressionNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        self.println_indented("Comma Expression");
        self.indent += 2;
        for expr in &mut node.value {
            let _ = expr.visit(self, cell_key);
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_decl(&mut self, node: &mut DeclNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Decl");
        self.indent += 2;
        self.println_indented(&format!("type: {}", node.type_));
        self.println_indented("initializations:");
        self.indent += 2;
        for init in &mut node.initializations {
            init.visit(self, cell_key)?;
        }
        self.indent -= 4;

        Ok(())
    }

    fn visit_do_while(&mut self, node: &mut DoWhileNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Do");
        self.indent += 2;
        let _ = node.body.visit(self, cell_key);
        self.indent -= 2;
        self.println_indented("while:");
        self.indent += 2;
        let _ = node.condition.visit(self, cell_key);
        self.indent -= 2;

        Ok(())
    }

    fn visit_float(&mut self, node: &mut FloatNode, _cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented(&format!("Float: {}", node.value));

        Ok(())
    }

    fn visit_for(&mut self, node: &mut ForNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        self.println_indented("For:");
        self.indent += 2;
        self.println_indented("init:");
        self.indent += 2;
        if let Some(init) = &mut *node.initializer {
            init.visit(self, cell_key)?;
        }
        self.indent -= 2;
        self.println_indented("condition:");
        self.indent += 2;
        if let Some(cond) = &mut node.condition {
            cond.visit(self, cell_key)?;
        }
        self.indent -= 2;
        self.println_indented("incrementer:");
        self.indent += 2;
        if let Some(incr) = &mut node.incrementer {
            incr.visit(self, cell_key)?;
        }

        self.indent -= 2;

        self.println_indented("body:");

        self.indent += 2;

        node.body.visit(self, cell_key)?;

        self.indent -= 4;

        Ok(())
    }

    fn visit_foreach(&mut self, node: &mut ForEachNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        self.println_indented("Foreach:");
        self.indent += 2;
        self.println_indented(&format!("init: {}", node.initializer));
        self.println_indented("collection:");
        node.collection.visit(self, cell_key)?;

        self.indent += 2;
        node.body.visit(self, cell_key)?;

        self.indent -= 4;

        Ok(())
    }

    fn visit_function_def(
        &mut self,
        node: &mut FunctionDefNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        self.println_indented("Function Def");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("return type: {}", node.return_type));
        self.println_indented("parameters:");
        self.indent += 2;
        for parameter in &mut node.parameters {
            parameter.visit(self, cell_key)?;
        }
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        for expression in &mut node.body {
            expression.visit(self, cell_key)?;
        }
        self.indent -= 4;

        Ok(())
    }

    fn visit_function_ptr(
        &mut self,
        node: &mut FunctionPtrNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        self.println_indented("Function Ptr");
        self.indent += 2;

        if let Some(rcvr) = &mut node.receiver {
            self.println_indented(&format!("receiver: {rcvr}"));
        } else {
            self.println_indented("receiver: None");
        }

        self.println_indented(&format!("name: {}", node.name));

        self.println_indented("arguments:");
        self.indent += 2;

        if let Some(args) = &mut node.arguments {
            for argument in args {
                if let Some(n) = argument {
                    n.visit(self, cell_key)?;
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

    fn visit_if(&mut self, node: &mut IfNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("If");
        self.indent += 2;
        let _ = node.condition.visit(self, cell_key);
        self.indent -= 2;
        self.println_indented("then");
        self.indent += 2;
        let _ = node.body.visit(self, cell_key);
        self.indent -= 2;

        if let Some(n) = &mut *node.else_clause {
            self.println_indented("else");
            self.indent += 2;

            let _ = n.visit(self, cell_key);
            self.indent -= 2;
        }

        Ok(())
    }

    fn visit_inherit(&mut self, node: &mut InheritNode, _cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Inherit:");

        self.indent += 2;

        self.println_indented(&node.path);

        if let Some(namespace) = &node.namespace {
            self.println_indented(namespace);
        }

        self.indent -= 2;

        Ok(())
    }

    fn visit_int(&mut self, node: &mut IntNode, _cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented(&format!("Int: {}", node.value));

        Ok(())
    }

    fn visit_mapping(&mut self, node: &mut MappingNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Mapping ([");
        self.indent += 2;
        for (key, value) in &mut node.value {
            key.visit(self, cell_key)?;
            self.println_indented(": ");
            value.visit(self, cell_key)?;
        }
        self.indent -= 2;
        self.println_indented("])");

        Ok(())
    }

    fn visit_program(&mut self, node: &mut ProgramNode, cell_key: &mut QCellOwner) -> Result<()> {
        println!("Program");
        self.indent += 2;
        for expr in &mut node.inherits {
            expr.visit(self, cell_key)?;
        }

        for expr in &mut node.body {
            expr.visit(self, cell_key)?;
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_range(&mut self, node: &mut RangeNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Range");

        self.println_indented("l: ");
        self.indent += 2;
        if let Some(expr) = &mut *node.l {
            expr.visit(self, cell_key)?;
        } else {
            self.println_indented("None");
        }
        self.indent -= 2;

        self.println_indented("r: ");
        self.indent += 2;
        if let Some(expr) = &mut *node.r {
            expr.visit(self, cell_key)?;
        } else {
            self.println_indented("None");
        }

        self.indent -= 2;

        Ok(())
    }

    fn visit_return(&mut self, node: &mut ReturnNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Return");
        self.indent += 2;
        if let Some(expression) = &mut node.value {
            expression.visit(self, cell_key)?;
        }
        self.indent -= 2;

        Ok(())
    }

    fn visit_string(&mut self, node: &mut StringNode, _cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented(&format!("String: {}", node.value));

        Ok(())
    }

    fn visit_unary_op(&mut self, node: &mut UnaryOpNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Unary Op");
        self.indent += 2;
        self.println_indented(&format!("operation: {:?}", node.op));
        self.println_indented("expr: ");
        self.indent += 2;
        node.expr.visit(self, cell_key)?;
        self.indent -= 4;

        Ok(())
    }

    fn visit_var(&mut self, node: &mut VarNode, _cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("Var");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.indent -= 2;

        Ok(())
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("VarInit");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("type: {}", node.type_));
        if let Some(node) = &mut node.value {
            self.println_indented("value:");
            self.indent += 2;
            node.visit(self, cell_key)?;
            self.indent -= 2;
        } else {
            self.println_indented("value: None");
        }
        self.println_indented(&format!("array: {}", node.array));
        self.indent -= 2;

        Ok(())
    }

    fn visit_while(&mut self, node: &mut WhileNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.println_indented("While");
        self.indent += 2;
        let _ = node.condition.visit(self, cell_key);
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        let _ = node.body.visit(self, cell_key);
        self.indent -= 2;

        Ok(())
    }
}
