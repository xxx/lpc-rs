use async_trait::async_trait;
use lpc_rs_errors::Result;
use tree_walker::{
    TreeWalker, walk_array, walk_block, walk_comma_expression, walk_decl, walk_label, walk_program,
    walk_return, walk_unary_op,
};

use crate::compiler::{
    ast::{
        array_node::ArrayNode,
        assignment_node::AssignmentNode,
        ast_node::AstNodeTrait,
        binary_op_node::BinaryOpNode,
        block_node::BlockNode,
        break_node::BreakNode,
        call_node::{CallChain, CallNode},
        closure_node::ClosureNode,
        comma_expression_node::CommaExpressionNode,
        continue_node::ContinueNode,
        decl_node::DeclNode,
        do_while_node::DoWhileNode,
        float_node::FloatNode,
        for_each_node::{ForEachInit, ForEachNode},
        for_node::ForNode,
        function_def_node::FunctionDefNode,
        function_ptr_node::FunctionPtrNode,
        if_node::IfNode,
        inherit_node::InheritNode,
        int_node::IntNode,
        label_node::LabelNode,
        mapping_node::MappingNode,
        program_node::ProgramNode,
        range_node::RangeNode,
        return_node::ReturnNode,
        string_node::StringNode,
        switch_node::SwitchNode,
        ternary_node::TernaryNode,
        unary_op_node::UnaryOpNode,
        var_init_node::VarInitNode,
        var_node::VarNode,
        while_node::WhileNode,
    },
    codegen::tree_walker,
};

/// A tree walker for pretty-printing an AST
///
/// # Examples
/// ```
/// # tokio_test::block_on(async {
/// use lpc_rs::{
///     compiler::{
///         codegen::{tree_printer::TreePrinter, tree_walker::TreeWalker},
///         compilation_context::CompilationContext,
///         lexer::LexWrapper,
///     },
///     lpc_parser,
/// };
///
/// let prog = "int main() { int b = 123; return b; }";
/// let lexer = LexWrapper::new(prog, 0);
/// let mut program_node = lpc_parser::ProgramParser::new()
///     .parse(&mut CompilationContext::default(), lexer)
///     .unwrap();
/// let mut walker = TreePrinter::new();
/// walker
///     .visit_program(&mut program_node)
///     .await
///     .expect("error walking the tree");
/// print!("{}", walker.into_output());
/// # });
/// ```
#[derive(Debug)]
pub struct TreePrinter {
    indent: usize,
    output: String,
}

impl TreePrinter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            output: String::new(),
        }
    }

    /// Consume this printer, and return the rendered tree.
    pub fn into_output(self) -> String {
        self.output
    }

    fn println_indented(&mut self, output: &str) {
        use std::fmt::Write as _;

        let _ = writeln!(self.output, "{:width$}{}", "", output, width = self.indent);
    }
}

impl Default for TreePrinter {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl TreeWalker for TreePrinter {
    async fn visit_array(&mut self, node: &mut ArrayNode) -> Result<()> {
        self.println_indented("Array ({");
        self.indent += 2;
        walk_array(self, node).await?;
        self.indent -= 2;
        self.println_indented("})");

        Ok(())
    }

    async fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()> {
        self.println_indented("Assignment");
        self.indent += 2;
        self.println_indented("lhs:");
        self.indent += 2;
        node.lhs.visit(self).await?;
        self.indent -= 2;
        self.println_indented("rhs:");
        self.indent += 2;
        node.rhs.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()> {
        self.println_indented("Binary Op");
        self.indent += 2;
        self.println_indented(&format!("operation: {:?}", node.op));
        self.println_indented("l: ");
        self.indent += 2;
        node.l.visit(self).await?;
        self.indent -= 2;
        self.println_indented("r: ");
        self.indent += 2;
        node.r.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        self.println_indented("Block {");

        self.indent += 2;

        walk_block(self, node).await?;

        self.indent -= 2;

        self.println_indented("}");

        Ok(())
    }

    async fn visit_break(&mut self, _node: &mut BreakNode) -> Result<()> {
        self.println_indented("Break");

        Ok(())
    }

    async fn visit_call(&mut self, node: &mut CallNode) -> Result<()> {
        match node.chain {
            CallChain::Root {
                ref receiver,
                ref name,
                ..
            } => {
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
                    arg.visit(self).await?;
                }
                self.indent -= 4;
            }
            CallChain::Node(ref mut chain_node) => {
                chain_node.visit(self).await?;
                self.indent += 2;
                self.println_indented("chain args:");
                self.indent += 2;
                for arg in &mut node.arguments {
                    arg.visit(self).await?;
                }
                self.indent -= 4;
            }
        }

        Ok(())
    }

    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        self.println_indented("Closure");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("return type: {}", node.return_type));
        self.println_indented("parameters:");
        self.indent += 2;
        if let Some(parameters) = &mut node.parameters {
            for parameter in parameters {
                parameter.visit(self).await?;
            }
        }
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        for expression in &mut node.body {
            expression.visit(self).await?;
        }
        self.indent -= 4;

        Ok(())
    }

    async fn visit_comma_expression(&mut self, node: &mut CommaExpressionNode) -> Result<()> {
        self.println_indented("Comma Expression");
        self.indent += 2;
        walk_comma_expression(self, node).await?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_continue(&mut self, _node: &mut ContinueNode) -> Result<()> {
        self.println_indented("Continue");

        Ok(())
    }

    async fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()> {
        self.println_indented("Decl");
        self.indent += 2;
        self.println_indented(&format!("type: {}", node.type_));
        self.println_indented("initializations:");
        self.indent += 2;
        walk_decl(self, node).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        self.println_indented("Do");
        self.indent += 2;
        let _ = node.body.visit(self).await;
        self.indent -= 2;
        self.println_indented("while:");
        self.indent += 2;
        let _ = node.condition.visit(self).await;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_float(&mut self, node: &mut FloatNode) -> Result<()> {
        self.println_indented(&format!("Float: {}", node.value));

        Ok(())
    }

    async fn visit_for(&mut self, node: &mut ForNode) -> Result<()>
    where
        Self: Sized,
    {
        self.println_indented("For:");
        self.indent += 2;
        self.println_indented("init:");
        self.indent += 2;
        if let Some(init) = &mut *node.initializer {
            init.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented("condition:");
        self.indent += 2;
        if let Some(cond) = &mut node.condition {
            cond.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented("incrementer:");
        self.indent += 2;
        if let Some(incr) = &mut node.incrementer {
            incr.visit(self).await?;
        }

        self.indent -= 2;

        self.println_indented("body:");

        self.indent += 2;

        node.body.visit(self).await?;

        self.indent -= 4;

        Ok(())
    }

    async fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()> {
        self.println_indented("Foreach:");
        self.indent += 2;
        self.println_indented("init:");
        self.indent += 2;
        match &mut node.initializer {
            ForEachInit::Array(init) | ForEachInit::String(init) => {
                init.visit(self).await?;
            }
            ForEachInit::Mapping { key, value } => {
                key.visit(self).await?;
                value.visit(self).await?;
            }
        }
        self.indent -= 2;
        self.println_indented("collection:");
        self.indent += 2;
        node.collection.visit(self).await?;
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        self.println_indented("Function Def");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("return type: {}", node.return_type));
        self.println_indented("parameters:");
        self.indent += 2;
        for parameter in &mut node.parameters {
            parameter.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        for expression in &mut node.body {
            expression.visit(self).await?;
        }
        self.indent -= 4;

        Ok(())
    }

    async fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()> {
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
                    n.visit(self).await?;
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

    async fn visit_if(&mut self, node: &mut IfNode) -> Result<()> {
        self.println_indented("If");
        self.indent += 2;
        let _ = node.condition.visit(self).await;
        self.indent -= 2;
        self.println_indented("then");
        self.indent += 2;
        let _ = node.body.visit(self).await;
        self.indent -= 2;

        if let Some(n) = &mut *node.else_clause {
            self.println_indented("else");
            self.indent += 2;

            let _ = n.visit(self).await;
            self.indent -= 2;
        }

        Ok(())
    }

    async fn visit_inherit(&mut self, node: &mut InheritNode) -> Result<()> {
        self.println_indented("Inherit:");

        self.indent += 2;

        self.println_indented(&node.path);

        if let Some(namespace) = &node.namespace {
            self.println_indented(namespace);
        }

        self.indent -= 2;

        Ok(())
    }

    async fn visit_int(&mut self, node: &mut IntNode) -> Result<()> {
        self.println_indented(&format!("Int: {}", node.value));

        Ok(())
    }

    async fn visit_label(&mut self, node: &mut LabelNode) -> Result<()> {
        if node.is_default() {
            self.println_indented("Default");
        } else {
            self.println_indented("Case:");
            self.indent += 2;
            walk_label(self, node).await?;
            self.indent -= 2;
        }

        Ok(())
    }

    async fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()> {
        self.println_indented("Mapping ([");
        self.indent += 2;
        for (key, value) in &mut node.value {
            key.visit(self).await?;
            self.println_indented(": ");
            value.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented("])");

        Ok(())
    }

    async fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()> {
        self.println_indented("Program");
        self.indent += 2;
        walk_program(self, node).await?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_range(&mut self, node: &mut RangeNode) -> Result<()> {
        self.println_indented("Range");

        self.println_indented("l: ");
        self.indent += 2;
        if let Some(expr) = &mut *node.l {
            expr.visit(self).await?;
        } else {
            self.println_indented("None");
        }
        self.indent -= 2;

        self.println_indented("r: ");
        self.indent += 2;
        if let Some(expr) = &mut *node.r {
            expr.visit(self).await?;
        } else {
            self.println_indented("None");
        }

        self.indent -= 2;

        Ok(())
    }

    async fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()> {
        self.println_indented("Return");
        self.indent += 2;
        walk_return(self, node).await?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_string(&mut self, node: &mut StringNode) -> Result<()> {
        self.println_indented(&format!("String: {}", node.value));

        Ok(())
    }

    async fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()> {
        self.println_indented("Switch");
        self.indent += 2;
        self.println_indented("expression:");
        self.indent += 2;
        node.expression.visit(self).await?;
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()> {
        self.println_indented("Ternary");
        self.indent += 2;
        self.println_indented("condition:");
        self.indent += 2;
        node.condition.visit(self).await?;
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 2;
        self.println_indented("else:");
        self.indent += 2;
        node.else_clause.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()> {
        self.println_indented("Unary Op");
        self.indent += 2;
        self.println_indented(&format!("operation: {:?}", node.op));
        self.println_indented("expr: ");
        self.indent += 2;
        walk_unary_op(self, node).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        self.println_indented("Var");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.indent -= 2;

        Ok(())
    }

    async fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        self.println_indented("VarInit");
        self.indent += 2;
        self.println_indented(&format!("name: {}", node.name));
        self.println_indented(&format!("type: {}", node.type_));
        if let Some(node) = &mut node.value {
            self.println_indented("value:");
            self.indent += 2;
            node.visit(self).await?;
            self.indent -= 2;
        } else {
            self.println_indented("value: None");
        }
        self.println_indented(&format!("array: {}", node.array));
        self.indent -= 2;

        Ok(())
    }

    async fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        self.println_indented("While");
        self.indent += 2;
        let _ = node.condition.visit(self).await;
        self.indent -= 2;
        self.println_indented("body:");
        self.indent += 2;
        let _ = node.body.visit(self).await;
        self.indent -= 2;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use lpc_rs_core::lpc_path::LpcPath;

    use super::*;
    use crate::compiler::Compiler;

    #[tokio::test]
    async fn every_node_prints_itself_and_its_children() {
        let code = indoc! {r#"
            int a = 0;
            void marf() {
                a = 2;
                int t = a > 1 ? 3 : 4;
                foreach(i: ({ 5 })) {
                    continue;
                }
                switch (a) {
                    case 6:
                        break;
                    default:
                        break;
                }
                function f = (: $1 :);
            }
        "#};

        let (mut program_node, _context) = Compiler::default()
            .parse_string(
                &LpcPath::new_in_game("/my_test.c", "/", "./tests/fixtures/code"),
                code,
            )
            .await
            .expect("failed to parse");

        let mut walker = TreePrinter::new();
        walker.visit_program(&mut program_node).await.unwrap();

        let expected = indoc! {r#"
            Program
              Decl
                type: int
                initializations:
                  VarInit
                    name: a
                    type: int
                    value:
                      Int: 0
                    array: false
              Function Def
                name: marf
                return type: void
                parameters:
                body:
                  Assignment
                    lhs:
                      Var
                        name: a
                    rhs:
                      Int: 2
                  Decl
                    type: int
                    initializations:
                      VarInit
                        name: t
                        type: int
                        value:
                          Ternary
                            condition:
                              Binary Op
                                operation: Gt
                                l: 
                                  Var
                                    name: a
                                r: 
                                  Int: 1
                            body:
                              Int: 3
                            else:
                              Int: 4
                        array: false
                  Foreach:
                    init:
                      VarInit
                        name: i
                        type: mixed
                        value: None
                        array: false
                    collection:
                      Array ({
                        Int: 5
                      })
                    body:
                      Block {
                        Continue
                      }
                  Switch
                    expression:
                      Var
                        name: a
                    body:
                      Block {
                        Case:
                          Int: 6
                        Break
                        Default
                        Break
                      }
                  Decl
                    type: function
                    initializations:
                      VarInit
                        name: f
                        type: function
                        value:
                          Closure
                            name: closure-0
                            return type: mixed
                            parameters:
                            body:
                              Var
                                name: $1
                        array: false
        "#};

        assert_eq!(walker.into_output(), expected);
    }
}
