use std::fmt::{self, Write};

use async_trait::async_trait;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::string::MAX_STRING_LENGTH;
use tree_walker::{
    TreeWalker, walk_array, walk_block, walk_cast, walk_decl, walk_label, walk_program,
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
        capture_node::CaptureNode,
        cast_node::CastNode,
        catch_node::CatchNode,
        class_def_node::ClassDefNode,
        class_expression::{ClassInitNode, CompoundAssignmentNode, MemberNode},
        closure_node::ClosureNode,
        comma_expression_node::CommaExpressionNode,
        continue_node::ContinueNode,
        decl_node::DeclNode,
        do_while_node::DoWhileNode,
        float_node::FloatNode,
        for_each_node::{ForEachInit, ForEachNode},
        for_node::ForNode,
        function_def_node::FunctionDefNode,
        function_ptr_node::{FunctionPtrNode, FunctionPtrReceiver},
        if_node::IfNode,
        inherit_node::InheritNode,
        int_node::IntNode,
        label_node::LabelNode,
        mapping_node::MappingNode,
        nil_node::NilNode,
        operator_node::OperatorNode,
        program_node::ProgramNode,
        range_node::RangeNode,
        ref_node::RefNode,
        return_node::ReturnNode,
        spread_node::SpreadNode,
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

const MAX_TREE_LINES: usize = 65_536;
const MAX_TREE_BYTES: usize = 4 * 1024 * 1024;

/// A tree walker producing escaped diagnostic lines from an AST.
///
/// Rendering rejects lines over the LPC string limit, more than 65,536 lines,
/// or more than 4 MiB including line endings; it yields every 512 visited nodes.
///
/// # Examples
/// ```
/// # #[tokio::main(flavor = "current_thread")]
/// # async fn main() {
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
/// let lexer = LexWrapper::new(prog, 0).triples();
/// let mut program_node = lpc_parser::ProgramParser::new()
///     .parse(&mut CompilationContext::default(), lexer)
///     .unwrap();
/// let mut walker = TreePrinter::new();
/// walker
///     .visit_program(&mut program_node)
///     .await
///     .expect("error walking the tree");
/// print!("{}", walker.into_output());
/// # }
/// ```
#[derive(Debug)]
pub struct TreePrinter {
    indent: usize,
    lines: Vec<String>,
    bytes: usize,
    work: usize,
}

struct TreeLine(String);

impl fmt::Write for TreeLine {
    fn write_str(&mut self, text: &str) -> fmt::Result {
        if text.chars().any(char::is_control) {
            for character in text.chars() {
                if character.is_control() {
                    write!(self, "{}", character.escape_debug())?;
                } else {
                    self.write_char(character)?;
                }
            }
            return Ok(());
        }
        if text.len() > MAX_STRING_LENGTH - self.0.len() {
            return Err(fmt::Error);
        }
        self.0.push_str(text);
        Ok(())
    }
}

impl TreePrinter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            lines: Vec::new(),
            bytes: 0,
            work: 0,
        }
    }

    /// Consume the printer and join its lines with trailing newlines for host output.
    pub fn into_output(self) -> String {
        if self.lines.is_empty() {
            String::new()
        } else {
            self.lines.join("\n") + "\n"
        }
    }

    /// Consume the printer and return bounded diagnostic lines without newlines.
    pub fn into_lines(self) -> Vec<String> {
        self.lines
    }

    async fn checkpoint(&mut self) {
        self.work += 1;
        if self.work.is_multiple_of(512) {
            tokio::task::yield_now().await;
        }
    }

    fn println_indented(&mut self, output: fmt::Arguments<'_>) -> Result<()> {
        if self.lines.len() >= MAX_TREE_LINES {
            return Err(LpcError::runtime("syntax_tree: output exceeds 65536 lines"));
        }
        let mut line = TreeLine(String::new());
        write!(line, "{:width$}{output}", "", width = self.indent).map_err(|_| {
            LpcError::runtime("syntax_tree: output line exceeds maximum string length")
        })?;
        let bytes = line.0.len() + 1;
        if bytes > MAX_TREE_BYTES - self.bytes {
            return Err(LpcError::runtime("syntax_tree: output exceeds 4 MiB"));
        }
        self.bytes += bytes;
        self.lines.push(line.0);
        Ok(())
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
        self.checkpoint().await;
        self.println_indented(format_args!("Array ({{"))?;
        self.indent += 2;
        walk_array(self, node).await?;
        self.indent -= 2;
        self.println_indented(format_args!("}})"))?;

        Ok(())
    }

    async fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Assignment"))?;
        self.indent += 2;
        if node.is_compound {
            self.println_indented(format_args!("compound: true"))?;
        }
        self.println_indented(format_args!("lhs:"))?;
        self.indent += 2;
        node.lhs.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("rhs:"))?;
        self.indent += 2;
        node.rhs.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Binary Op"))?;
        self.indent += 2;
        self.println_indented(format_args!("operation: {:?}", node.op))?;
        self.println_indented(format_args!("l: "))?;
        self.indent += 2;
        node.l.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("r: "))?;
        self.indent += 2;
        node.r.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Block {{"))?;

        self.indent += 2;

        walk_block(self, node).await?;

        self.indent -= 2;

        self.println_indented(format_args!("}}"))?;

        Ok(())
    }

    async fn visit_catch(&mut self, node: &mut CatchNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Catch"))?;
        self.indent += 2;
        let result = node.body.visit(self).await;
        self.indent -= 2;
        result
    }

    async fn visit_break(&mut self, _node: &mut BreakNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Break"))?;

        Ok(())
    }

    async fn visit_call(&mut self, node: &mut CallNode) -> Result<()> {
        self.checkpoint().await;
        match node.chain {
            CallChain::Root {
                ref mut receiver,
                ref name,
                ref namespace,
            } => {
                if let Some(rcvr) = receiver {
                    self.println_indented(format_args!("Call Other"))?;
                    self.indent += 2;
                    self.println_indented(format_args!("receiver:"))?;
                    self.indent += 2;
                    rcvr.visit(self).await?;
                    self.indent -= 2;
                } else {
                    self.println_indented(format_args!("Call"))?;
                    self.indent += 2;
                }
                self.println_indented(format_args!("name: {}", name))?;
                if !namespace.as_str().is_empty() {
                    self.println_indented(format_args!("namespace: {namespace}"))?;
                }
                self.println_indented(format_args!("args:"))?;
                self.indent += 2;
                for arg in &mut node.arguments {
                    arg.visit(self).await?;
                }
                self.indent -= 4;
            }
            CallChain::Node(ref mut chain_node) => {
                chain_node.visit(self).await?;
                self.indent += 2;
                self.println_indented(format_args!("chain args:"))?;
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
        self.checkpoint().await;
        self.println_indented(format_args!("Closure"))?;
        self.indent += 2;
        self.println_indented(format_args!("name: {}", node.name))?;
        self.println_indented(format_args!("return type: {}", node.return_type))?;
        if node.flags != Default::default() {
            self.println_indented(format_args!("flags: {:?}", node.flags))?;
        }
        self.println_indented(format_args!("parameters:"))?;
        self.indent += 2;
        if let Some(parameters) = &mut node.parameters {
            for parameter in parameters {
                parameter.visit(self).await?;
            }
        }
        if let Some(collector) = &mut node.variadic {
            self.println_indented(format_args!("variadic:"))?;
            collector.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented(format_args!("body:"))?;
        self.indent += 2;
        for expression in &mut node.body {
            expression.visit(self).await?;
        }
        self.indent -= 4;

        Ok(())
    }

    async fn visit_comma_expression(&mut self, node: &mut CommaExpressionNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Comma Expression"))?;
        self.indent += 2;
        for expression in &mut node.value {
            expression.visit(self).await?;
        }
        self.indent -= 2;

        Ok(())
    }

    async fn visit_continue(&mut self, _node: &mut ContinueNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Continue"))?;

        Ok(())
    }

    async fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Decl"))?;
        self.indent += 2;
        self.println_indented(format_args!("type: {}", node.type_))?;
        self.println_indented(format_args!("initializations:"))?;
        self.indent += 2;
        walk_decl(self, node).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Do"))?;
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("while:"))?;
        self.indent += 2;
        node.condition.visit(self).await?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_float(&mut self, node: &mut FloatNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Float: {}", node.value))?;

        Ok(())
    }

    async fn visit_for(&mut self, node: &mut ForNode) -> Result<()>
    where
        Self: Sized,
    {
        self.checkpoint().await;
        self.println_indented(format_args!("For:"))?;
        self.indent += 2;
        self.println_indented(format_args!("init:"))?;
        self.indent += 2;
        if let Some(init) = &mut *node.initializer {
            init.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented(format_args!("condition:"))?;
        self.indent += 2;
        if let Some(cond) = &mut node.condition {
            cond.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented(format_args!("incrementer:"))?;
        self.indent += 2;
        if let Some(incr) = &mut node.incrementer {
            incr.visit(self).await?;
        }

        self.indent -= 2;

        self.println_indented(format_args!("body:"))?;

        self.indent += 2;

        node.body.visit(self).await?;

        self.indent -= 4;

        Ok(())
    }

    async fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Foreach:"))?;
        self.indent += 2;
        self.println_indented(format_args!("init:"))?;
        self.indent += 2;
        match &mut node.initializer {
            ForEachInit::Array(init) => {
                init.visit(self).await?;
            }
            ForEachInit::Mapping { key, value } => {
                key.visit(self).await?;
                value.visit(self).await?;
            }
        }
        self.indent -= 2;
        self.println_indented(format_args!("collection:"))?;
        self.indent += 2;
        node.collection.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("body:"))?;
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Function Def"))?;
        self.indent += 2;
        self.println_indented(format_args!("name: {}", node.name))?;
        self.println_indented(format_args!("return type: {}", node.return_type))?;
        if node.flags != Default::default() {
            self.println_indented(format_args!("flags: {:?}", node.flags))?;
        }
        self.println_indented(format_args!("parameters:"))?;
        self.indent += 2;
        for parameter in &mut node.parameters {
            parameter.visit(self).await?;
        }
        if let Some(collector) = &mut node.variadic {
            self.println_indented(format_args!("variadic:"))?;
            collector.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented(format_args!("body:"))?;
        self.indent += 2;
        for expression in &mut node.body {
            expression.visit(self).await?;
        }
        self.indent -= 4;

        Ok(())
    }

    async fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Function Ptr"))?;
        self.indent += 2;

        match &mut node.receiver {
            Some(FunctionPtrReceiver::Static(receiver)) => {
                self.println_indented(format_args!("receiver:"))?;
                self.indent += 2;
                receiver.visit(self).await?;
                self.indent -= 2;
            }
            Some(FunctionPtrReceiver::Dynamic) => {
                self.println_indented(format_args!("receiver: dynamic"))?
            }
            None => self.println_indented(format_args!("receiver: None"))?,
        }

        self.println_indented(format_args!("name: {}", node.name))?;

        self.println_indented(format_args!("arguments:"))?;
        self.indent += 2;

        if let Some(args) = &mut node.arguments {
            for argument in args {
                if let Some(n) = argument {
                    n.visit(self).await?;
                } else {
                    self.println_indented(format_args!("None"))?;
                }
            }
        } else {
            self.println_indented(format_args!("None"))?;
        }

        self.indent -= 4;

        Ok(())
    }

    async fn visit_if(&mut self, node: &mut IfNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("If"))?;
        self.indent += 2;
        node.condition.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("then"))?;
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 2;

        if let Some(n) = &mut *node.else_clause {
            self.println_indented(format_args!("else"))?;
            self.indent += 2;

            n.visit(self).await?;
            self.indent -= 2;
        }

        Ok(())
    }

    async fn visit_inherit(&mut self, node: &mut InheritNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Inherit:"))?;

        self.indent += 2;

        self.println_indented(format_args!("{:?}", node.path.as_str()))?;
        self.println_indented(format_args!("visibility: {}", node.visibility))?;

        if let Some(namespace) = &node.namespace {
            self.println_indented(format_args!("namespace: {namespace}"))?;
        }

        self.indent -= 2;

        Ok(())
    }

    async fn visit_int(&mut self, node: &mut IntNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Int: {}", node.value))?;

        Ok(())
    }

    async fn visit_label(&mut self, node: &mut LabelNode) -> Result<()> {
        self.checkpoint().await;
        if node.is_default() {
            self.println_indented(format_args!("Default"))?;
        } else {
            self.println_indented(format_args!("Case:"))?;
            self.indent += 2;
            walk_label(self, node).await?;
            self.indent -= 2;
        }

        Ok(())
    }

    async fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Mapping (["))?;
        self.indent += 2;
        for (key, value) in &mut node.value {
            key.visit(self).await?;
            self.println_indented(format_args!(": "))?;
            value.visit(self).await?;
        }
        self.indent -= 2;
        self.println_indented(format_args!("])"))?;

        Ok(())
    }

    async fn visit_operator(&mut self, node: &mut OperatorNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Operator {}", node.op))?;
        self.indent += 2;
        self.println_indented(format_args!("arguments:"))?;
        self.indent += 2;
        if let Some(args) = &mut node.arguments {
            for argument in args {
                match argument {
                    Some(n) => n.visit(self).await?,
                    None => self.println_indented(format_args!("None"))?,
                }
            }
        } else {
            self.println_indented(format_args!("None"))?;
        }
        self.indent -= 2;
        node.closure.visit(self).await?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Program"))?;
        self.indent += 2;
        walk_program(self, node).await?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_range(&mut self, node: &mut RangeNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Range"))?;

        self.println_indented(format_args!("l: "))?;
        self.indent += 2;
        if let Some(expr) = &mut *node.l {
            expr.visit(self).await?;
        } else {
            self.println_indented(format_args!("None"))?;
        }
        self.indent -= 2;

        self.println_indented(format_args!("r: "))?;
        self.indent += 2;
        if let Some(expr) = &mut *node.r {
            expr.visit(self).await?;
        } else {
            self.println_indented(format_args!("None"))?;
        }

        self.indent -= 2;

        Ok(())
    }

    async fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Return"))?;
        self.indent += 2;
        walk_return(self, node).await?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_string(&mut self, node: &mut StringNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("String: {:?}", node.value.as_str()))?;

        Ok(())
    }

    async fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Switch"))?;
        self.indent += 2;
        self.println_indented(format_args!("expression:"))?;
        self.indent += 2;
        node.expression.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("body:"))?;
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Ternary"))?;
        self.indent += 2;
        self.println_indented(format_args!("condition:"))?;
        self.indent += 2;
        node.condition.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("body:"))?;
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("else:"))?;
        self.indent += 2;
        node.else_clause.visit(self).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Unary Op"))?;
        self.indent += 2;
        self.println_indented(format_args!("operation: {:?}", node.op))?;
        if node.is_post {
            self.println_indented(format_args!("post: true"))?;
        }
        self.println_indented(format_args!("expr: "))?;
        self.indent += 2;
        walk_unary_op(self, node).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_cast(&mut self, node: &mut CastNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Cast"))?;
        self.indent += 2;
        self.println_indented(format_args!("type: {}", node.type_))?;
        self.println_indented(format_args!("expr: "))?;
        self.indent += 2;
        walk_cast(self, node).await?;
        self.indent -= 4;

        Ok(())
    }

    async fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Var"))?;
        self.indent += 2;
        self.println_indented(format_args!("name: {}", node.name))?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("VarInit"))?;
        self.indent += 2;
        self.println_indented(format_args!("name: {}", node.name))?;
        self.println_indented(format_args!("type: {}", node.type_))?;
        if let Some(node) = &mut node.value {
            self.println_indented(format_args!("value:"))?;
            self.indent += 2;
            node.visit(self).await?;
            self.indent -= 2;
        } else {
            self.println_indented(format_args!("value: None"))?;
        }
        self.println_indented(format_args!("array: {}", node.array))?;
        if node.by_ref {
            self.println_indented(format_args!("by_ref: true"))?;
        }
        if let Some(flags) = node.flags.filter(|flags| *flags != Default::default()) {
            self.println_indented(format_args!("flags: {flags}"))?;
        }
        self.indent -= 2;

        Ok(())
    }

    async fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("While"))?;
        self.indent += 2;
        node.condition.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("body:"))?;
        self.indent += 2;
        node.body.visit(self).await?;
        self.indent -= 2;

        Ok(())
    }

    async fn visit_class_def(&mut self, node: &mut ClassDefNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Class Def: {}", node.name))?;
        self.indent += 2;
        self.println_indented(format_args!("visibility: {}", node.visibility))?;
        for field in &node.fields {
            self.checkpoint().await;
            self.println_indented(format_args!("field: {} {}", field.type_, field.name))?;
        }
        self.indent -= 2;
        Ok(())
    }

    async fn visit_class_init(&mut self, node: &mut ClassInitNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Class Init: {}", node.type_))?;
        self.indent += 2;
        for (name, value) in &mut node.initializers {
            self.println_indented(format_args!("field: {name}"))?;
            self.indent += 2;
            value.visit(self).await?;
            self.indent -= 2;
        }
        self.indent -= 2;
        Ok(())
    }

    async fn visit_member(&mut self, node: &mut MemberNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Member: {}", node.name))?;
        self.indent += 2;
        node.receiver.visit(self).await?;
        self.indent -= 2;
        Ok(())
    }

    async fn visit_compound_assignment(&mut self, node: &mut CompoundAssignmentNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Compound Assignment: {}", node.op))?;
        self.indent += 2;
        self.println_indented(format_args!("lhs:"))?;
        self.indent += 2;
        node.lhs.visit(self).await?;
        self.indent -= 2;
        self.println_indented(format_args!("rhs:"))?;
        self.indent += 2;
        node.rhs.visit(self).await?;
        self.indent -= 4;
        Ok(())
    }

    async fn visit_capture(&mut self, node: &mut CaptureNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Capture: {}", node.name))?;
        self.indent += 2;
        node.expr.visit(self).await?;
        self.indent -= 2;
        Ok(())
    }

    async fn visit_capture_initializer(&mut self, node: &mut VarInitNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Capture Init"))?;
        self.indent += 2;
        self.visit_var_init(node).await?;
        self.indent -= 2;
        Ok(())
    }

    async fn visit_nil(&mut self, _node: &mut NilNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Nil"))
    }

    async fn visit_ref(&mut self, node: &mut RefNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Ref: {}", node.name))
    }

    async fn visit_spread(&mut self, node: &mut SpreadNode) -> Result<()> {
        self.checkpoint().await;
        self.println_indented(format_args!("Spread"))?;
        self.indent += 2;
        node.expr.visit(self).await?;
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
    async fn prints_declarations_control_flow_and_closures() {
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
                            flags: FunctionFlags { ellipsis: false, varargs: false, nomask: false, visibility: Private }
                            parameters:
                            body:
                              Var
                                name: $1
                        array: false
        "#};

        assert_eq!(walker.into_output(), expected);
    }
}

#[cfg(test)]
mod diagnostic_tests {
    use super::*;
    use crate::compiler::Compiler;
    use lpc_rs_core::lpc_path::LpcPath;

    async fn parsed(code: &str) -> ProgramNode {
        Compiler::default()
            .parse_string(
                &LpcPath::new_in_game("/tree.c", "/", "./tests/fixtures/code"),
                code,
            )
            .await
            .unwrap()
            .0
    }

    #[tokio::test]
    async fn retained_classes_nil_references_spreads_and_captures_are_visible() {
        let mut tree = parsed(
            r#"
            private class Record { int field; }
            void change(int ref value) { value++; }
            function f(mixed *xs) {
                class Record value = new(class Record, field: 1);
                value.field += 2;
                int counter = 0;
                counter += 1;
                change(ref value);
                mixed *copy = ({ xs..., counter++ });
                return (: $(value.field), nil :);
            }
        "#,
        )
        .await;
        let mut printer = TreePrinter::new();
        printer.visit_program(&mut tree).await.unwrap();
        let output = printer.into_output();
        for fragment in [
            "Class Def: Record",
            "visibility: private",
            "field: int field",
            "Class Init:",
            "field: field",
            "Member: field",
            "Compound Assignment: +",
            "compound: true",
            "post: true",
            "by_ref: true",
            "Ref: value",
            "Spread",
            "Capture:",
            "Nil",
        ] {
            assert!(output.contains(fragment), "missing {fragment}: {output}");
        }
    }

    #[tokio::test]
    async fn literal_controls_are_escaped_within_one_diagnostic_line() {
        let mut node = StringNode::new("line\n\r\t\0\x1b\"\\雪");
        let mut printer = TreePrinter::new();
        printer.visit_string(&mut node).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            vec![r#"String: "line\n\r\t\0\u{1b}\"\\雪""#.to_owned()]
        );
    }

    #[test]
    fn line_limits_count_utf8_bytes_and_indentation_before_appending() {
        let mut printer = TreePrinter::new();
        let boundary = "🦀".repeat(MAX_STRING_LENGTH / 4);
        printer
            .println_indented(format_args!("{boundary}"))
            .unwrap();
        printer.indent = 1;
        let error = printer
            .println_indented(format_args!("{boundary}"))
            .unwrap_err();
        assert!(error.to_string().contains("output line exceeds"));
        assert_eq!(printer.into_lines(), vec![boundary]);
    }

    #[test]
    fn total_bytes_and_line_count_are_bounded_before_appending() {
        let mut printer = TreePrinter::new();
        printer.bytes = MAX_TREE_BYTES - 3;
        assert!(
            printer
                .println_indented(format_args!("abc"))
                .unwrap_err()
                .to_string()
                .contains("4 MiB")
        );
        assert!(printer.lines.is_empty());
        printer.bytes = 0;
        printer.lines = vec![String::new(); MAX_TREE_LINES];
        assert!(
            printer
                .println_indented(format_args!("x"))
                .unwrap_err()
                .to_string()
                .contains("65536 lines")
        );
        assert_eq!(printer.lines.len(), MAX_TREE_LINES);
    }

    #[tokio::test]
    async fn nested_control_flow_and_comma_expressions_propagate_render_errors() {
        let long = "x".repeat(MAX_STRING_LENGTH);
        for body in [
            format!("if (1) return \"{long}\";"),
            format!("while (1) return \"{long}\";"),
            format!("do {{ return \"{long}\"; }} while (1);"),
            format!("return (\"{long}\", 1);"),
        ] {
            let mut tree = parsed(&format!("mixed f() {{ {body} }}")).await;
            let mut printer = TreePrinter::new();
            let error = printer.visit_program(&mut tree).await.unwrap_err();
            assert!(error.to_string().contains("output line exceeds"), "{error}");
        }
    }

    #[tokio::test]
    async fn a_large_tree_yields_before_it_finishes_rendering() {
        let source = format!(
            "mixed f() {{ {} return 1; }}",
            (0..1000)
                .map(|n| format!("int local_{n} = {n}; "))
                .collect::<String>()
        );
        let mut tree = parsed(&source).await;
        let mut printer = TreePrinter::new();
        let mut render = Box::pin(printer.visit_program(&mut tree));
        let first_poll = std::future::poll_fn(|context| {
            std::task::Poll::Ready(std::future::Future::poll(render.as_mut(), context))
        })
        .await;
        assert!(first_poll.is_pending());
        drop(render);
        assert!(!printer.lines.is_empty());
        assert!(printer.work >= 512);
    }

    #[test]
    fn the_documented_byte_budget_accepts_four_mebibytes_then_rejects_more() {
        let mut printer = TreePrinter::new();
        let line = "x".repeat(1023);
        for _ in 0..4096 {
            printer.println_indented(format_args!("{line}")).unwrap();
        }

        let error = printer.println_indented(format_args!("")).unwrap_err();

        assert!(error.to_string().contains("4 MiB"), "{error}");
        assert_eq!(printer.into_output().len(), 4_194_304);
    }

    #[tokio::test]
    async fn catch_blocks_render_their_body_and_restore_indentation() {
        let mut printer = TreePrinter::new();
        let mut node = CatchNode {
            body: BlockNode::new(vec![]),
            span: None,
        };
        printer.visit_catch(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(7)).await.unwrap();

        assert_eq!(
            printer.into_lines(),
            ["Catch", "  Block {", "  }", "Int: 7"]
        );
    }

    #[tokio::test]
    async fn calls_render_receivers_namespaces_and_chains_with_stable_indentation() {
        use crate::compiler::ast::expression_node::ExpressionNode;
        use lpc_rs_core::call_namespace::CallNamespace;
        use ustr::ustr;

        fn call(
            namespace: CallNamespace,
            receiver: Option<ExpressionNode>,
            name: &str,
            arguments: Vec<ExpressionNode>,
        ) -> CallNode {
            CallNode {
                chain: CallChain::Root {
                    receiver: receiver.map(Box::new),
                    namespace,
                    name: ustr(name),
                },
                arguments,
                span: None,
                lvalue_temps: vec![],
            }
        }

        for (namespace, line) in [
            (CallNamespace::Local, None),
            (CallNamespace::Parent, Some("  namespace: ::")),
            (
                CallNamespace::Named(ustr("base")),
                Some("  namespace: base::"),
            ),
            (
                CallNamespace::Named(ustr("efun")),
                Some("  namespace: efun::"),
            ),
        ] {
            let mut node = call(namespace, None, "invoke", vec![IntNode::new(7).into()]);
            let mut printer = TreePrinter::new();
            printer.visit_call(&mut node).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            let mut expected = vec!["Call", "  name: invoke"];
            if let Some(line) = line {
                expected.push(line);
            }
            expected.extend(["  args:", "    Int: 7", "Int: 9"]);
            assert_eq!(printer.into_lines(), expected);
        }

        let mut node = call(
            CallNamespace::Local,
            Some(VarNode::new("target").into()),
            "invoke",
            vec![IntNode::new(7).into()],
        );
        let mut printer = TreePrinter::new();
        printer.visit_call(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Call Other",
                "  receiver:",
                "    Var",
                "      name: target",
                "  name: invoke",
                "  args:",
                "    Int: 7",
                "Int: 9",
            ]
        );

        let factory = call(CallNamespace::Local, None, "factory", vec![]);
        let mut node = CallNode {
            chain: CallChain::Node(Box::new(ExpressionNode::Call(factory))),
            arguments: vec![IntNode::new(7).into()],
            span: None,
            lvalue_temps: vec![],
        };
        let mut printer = TreePrinter::new();
        printer.visit_call(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Call",
                "  name: factory",
                "  args:",
                "  chain args:",
                "    Int: 7",
                "Int: 9",
            ]
        );
    }

    #[tokio::test]
    async fn comma_expressions_indent_values_and_restore_the_following_node() {
        let mut node = CommaExpressionNode {
            value: vec![IntNode::new(7).into(), IntNode::new(8).into()],
            span: None,
        };
        let mut printer = TreePrinter::new();
        printer.visit_comma_expression(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();

        assert_eq!(
            printer.into_lines(),
            ["Comma Expression", "  Int: 7", "  Int: 8", "Int: 9"]
        );
    }

    #[tokio::test]
    async fn do_while_renders_its_body_condition_and_following_indentation() {
        use crate::compiler::ast::ast_node::AstNode;

        let mut node = DoWhileNode::new(
            AstNode::Block(BlockNode::new(vec![])),
            IntNode::new(7).into(),
            None,
        );
        let mut printer = TreePrinter::new();
        printer.visit_do_while(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();

        assert_eq!(
            printer.into_lines(),
            ["Do", "  Block {", "  }", "while:", "  Int: 7", "Int: 9"]
        );
    }

    #[tokio::test]
    async fn for_loops_render_present_and_omitted_clauses_and_restore_indentation() {
        use crate::compiler::ast::ast_node::AstNode;

        for (mut node, expected) in [
            (
                ForNode::new(
                    Some(AstNode::Expression(IntNode::new(1).into())),
                    Some(IntNode::new(2).into()),
                    Some(IntNode::new(3).into()),
                    AstNode::Block(BlockNode::new(vec![])),
                    None,
                ),
                vec![
                    "For:",
                    "  init:",
                    "    Int: 1",
                    "  condition:",
                    "    Int: 2",
                    "  incrementer:",
                    "    Int: 3",
                    "  body:",
                    "    Block {",
                    "    }",
                    "Int: 9",
                ],
            ),
            (
                ForNode::new(
                    None,
                    None,
                    None,
                    AstNode::Block(BlockNode::new(vec![])),
                    None,
                ),
                vec![
                    "For:",
                    "  init:",
                    "  condition:",
                    "  incrementer:",
                    "  body:",
                    "    Block {",
                    "    }",
                    "Int: 9",
                ],
            ),
        ] {
            let mut printer = TreePrinter::new();
            printer.visit_for(&mut node).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            assert_eq!(printer.into_lines(), expected);
        }
    }

    #[tokio::test]
    async fn standalone_function_definitions_render_parameters_body_and_restore_indentation() {
        use crate::compiler::ast::ast_node::AstNode;

        let mut tree = parsed("int render(int value) { return value; }").await;
        let AstNode::FunctionDef(mut function) = tree.body.remove(0) else {
            panic!("expected a function definition")
        };
        let mut printer = TreePrinter::new();
        printer.visit_function_def(&mut function).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();

        assert_eq!(
            printer.into_lines(),
            [
                "Function Def",
                "  name: render",
                "  return type: int",
                "  parameters:",
                "    VarInit",
                "      name: value",
                "      type: int",
                "      value: None",
                "      array: false",
                "  body:",
                "    Return",
                "      Var",
                "        name: value",
                "Int: 9",
            ]
        );
    }

    #[tokio::test]
    async fn function_pointers_render_receiver_forms_bound_values_and_holes() {
        use ustr::ustr;

        for (mut node, expected) in [
            (
                FunctionPtrNode {
                    receiver: None,
                    arguments: None,
                    name: ustr("invoke"),
                    span: None,
                },
                vec![
                    "Function Ptr",
                    "  receiver: None",
                    "  name: invoke",
                    "  arguments:",
                    "    None",
                    "Int: 9",
                ],
            ),
            (
                FunctionPtrNode {
                    receiver: Some(FunctionPtrReceiver::Dynamic),
                    arguments: Some(vec![]),
                    name: ustr("invoke"),
                    span: None,
                },
                vec![
                    "Function Ptr",
                    "  receiver: dynamic",
                    "  name: invoke",
                    "  arguments:",
                    "Int: 9",
                ],
            ),
            (
                FunctionPtrNode {
                    receiver: Some(FunctionPtrReceiver::Static(Box::new(
                        VarNode::new("target").into(),
                    ))),
                    arguments: Some(vec![
                        Some(IntNode::new(7).into()),
                        None,
                        Some(IntNode::new(8).into()),
                    ]),
                    name: ustr("invoke"),
                    span: None,
                },
                vec![
                    "Function Ptr",
                    "  receiver:",
                    "    Var",
                    "      name: target",
                    "  name: invoke",
                    "  arguments:",
                    "    Int: 7",
                    "    None",
                    "    Int: 8",
                    "Int: 9",
                ],
            ),
        ] {
            let mut printer = TreePrinter::new();
            printer.visit_function_ptr(&mut node).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            assert_eq!(printer.into_lines(), expected);
        }
    }

    #[tokio::test]
    async fn conditionals_render_both_forms_and_restore_the_following_indentation() {
        use crate::compiler::ast::ast_node::AstNode;

        for (with_else, expected) in [
            (
                false,
                vec!["If", "  Int: 1", "then", "  Return", "    Int: 7", "Int: 9"],
            ),
            (
                true,
                vec![
                    "If",
                    "  Int: 1",
                    "then",
                    "  Return",
                    "    Int: 7",
                    "else",
                    "  Return",
                    "    Int: 8",
                    "Int: 9",
                ],
            ),
        ] {
            let alternative =
                with_else.then(|| AstNode::Return(ReturnNode::new(Some(IntNode::new(8).into()))));
            let mut node = IfNode::new(
                IntNode::new(1).into(),
                AstNode::Return(ReturnNode::new(Some(IntNode::new(7).into()))),
                alternative,
                None,
            );
            let mut printer = TreePrinter::new();
            printer.visit_if(&mut node).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            assert_eq!(printer.into_lines(), expected);
        }
    }

    #[tokio::test]
    async fn standalone_inherits_render_visibility_aliases_and_restore_indentation() {
        use lpc_rs_core::visibility::Visibility;
        use ustr::ustr;

        for (visibility, namespace, expected) in [
            (
                Visibility::Public,
                None,
                vec![
                    "Inherit:",
                    "  \"/parent\"",
                    "  visibility: public",
                    "Int: 9",
                ],
            ),
            (
                Visibility::Protected,
                Some(ustr("base")),
                vec![
                    "Inherit:",
                    "  \"/parent\"",
                    "  visibility: protected",
                    "  namespace: base",
                    "Int: 9",
                ],
            ),
            (
                Visibility::Private,
                None,
                vec![
                    "Inherit:",
                    "  \"/parent\"",
                    "  visibility: private",
                    "Int: 9",
                ],
            ),
        ] {
            let mut node = InheritNode {
                path: ustr("/parent"),
                namespace,
                visibility,
                span: None,
            };
            let mut printer = TreePrinter::new();
            printer.visit_inherit(&mut node).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            assert_eq!(printer.into_lines(), expected);
        }
    }

    #[tokio::test]
    async fn mappings_render_each_key_value_pair_and_restore_indentation() {
        let mut node = MappingNode::new(
            vec![
                (IntNode::new(1).into(), IntNode::new(7).into()),
                (IntNode::new(2).into(), IntNode::new(8).into()),
            ],
            None,
        );
        let mut printer = TreePrinter::new();
        printer.visit_mapping(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();

        assert_eq!(
            printer.into_lines(),
            [
                "Mapping ([",
                "  Int: 1",
                "  : ",
                "  Int: 7",
                "  Int: 2",
                "  : ",
                "  Int: 8",
                "])",
                "Int: 9",
            ]
        );
    }

    #[tokio::test]
    async fn operator_functions_render_binding_holes_and_their_synthetic_body() {
        use crate::compiler::ast::{binary_op_node::BinaryOperation, operator_node::Operator};
        use ustr::ustr;

        for (arguments, argument_lines) in [
            (None, vec!["    None"]),
            (
                Some(vec![Some(IntNode::new(7).into()), None]),
                vec!["    Int: 7", "    None"],
            ),
        ] {
            let mut node = OperatorNode::new(
                Operator::Binary(BinaryOperation::Add),
                arguments,
                ustr("addition"),
                None,
            );
            let mut printer = TreePrinter::new();
            printer.visit_operator(&mut node).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            let mut expected = vec!["Operator +", "  arguments:"];
            expected.extend(argument_lines);
            expected.extend([
                "  Closure", "    name: addition", "    return type: mixed",
                "    flags: FunctionFlags { ellipsis: false, varargs: false, nomask: false, visibility: Private }",
                "    parameters:", "    body:", "      Binary Op", "        operation: Add", "        l: ",
                "          Var", "            name: $1", "        r: ", "          Var", "            name: $2", "Int: 9",
            ]);
            assert_eq!(printer.into_lines(), expected);
        }
    }

    #[tokio::test]
    async fn a_printer_can_render_separate_programs_at_the_same_root_indentation() {
        let mut printer = TreePrinter::new();
        printer
            .visit_program(&mut ProgramNode::default())
            .await
            .unwrap();
        printer
            .visit_program(&mut ProgramNode::default())
            .await
            .unwrap();
        assert_eq!(printer.into_lines(), ["Program", "Program"]);
    }

    #[tokio::test]
    async fn ranges_render_open_closed_and_reverse_bounds_and_restore_indentation() {
        use crate::compiler::ast::{
            expression_node::ExpressionNode, unary_op_node::UnaryOperation,
        };

        let reverse = |value| {
            ExpressionNode::UnaryOp(UnaryOpNode {
                expr: Box::new(IntNode::new(value).into()),
                op: UnaryOperation::FromEnd,
                is_post: false,
                span: None,
            })
        };
        for (mut node, expected) in [
            (
                RangeNode::new(None, None, None),
                vec!["Range", "l: ", "  None", "r: ", "  None", "Int: 9"],
            ),
            (
                RangeNode::new(Some(IntNode::new(1).into()), None, None),
                vec!["Range", "l: ", "  Int: 1", "r: ", "  None", "Int: 9"],
            ),
            (
                RangeNode::new(None, Some(IntNode::new(7).into()), None),
                vec!["Range", "l: ", "  None", "r: ", "  Int: 7", "Int: 9"],
            ),
            (
                RangeNode::new(
                    Some(IntNode::new(1).into()),
                    Some(IntNode::new(7).into()),
                    None,
                ),
                vec!["Range", "l: ", "  Int: 1", "r: ", "  Int: 7", "Int: 9"],
            ),
            (
                RangeNode::new(Some(reverse(1)), Some(reverse(2)), None),
                vec![
                    "Range",
                    "l: ",
                    "  Unary Op",
                    "    operation: FromEnd",
                    "    expr: ",
                    "      Int: 1",
                    "r: ",
                    "  Unary Op",
                    "    operation: FromEnd",
                    "    expr: ",
                    "      Int: 2",
                    "Int: 9",
                ],
            ),
        ] {
            let mut printer = TreePrinter::new();
            printer.visit_range(&mut node).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            assert_eq!(printer.into_lines(), expected);
        }
    }

    #[tokio::test]
    async fn standalone_unary_expressions_render_prefix_and_postfix_forms() {
        use crate::compiler::ast::unary_op_node::UnaryOperation;

        for (mut node, expected) in [
            (
                UnaryOpNode {
                    expr: Box::new(IntNode::new(7).into()),
                    op: UnaryOperation::Negate,
                    is_post: false,
                    span: None,
                },
                vec![
                    "Unary Op",
                    "  operation: Negate",
                    "  expr: ",
                    "    Int: 7",
                    "Int: 9",
                ],
            ),
            (
                UnaryOpNode {
                    expr: Box::new(VarNode::new("value").into()),
                    op: UnaryOperation::Inc,
                    is_post: true,
                    span: None,
                },
                vec![
                    "Unary Op",
                    "  operation: Inc",
                    "  post: true",
                    "  expr: ",
                    "    Var",
                    "      name: value",
                    "Int: 9",
                ],
            ),
        ] {
            let mut printer = TreePrinter::new();
            printer.visit_unary_op(&mut node).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            assert_eq!(printer.into_lines(), expected);
        }
    }

    #[tokio::test]
    async fn casts_render_the_type_and_operand_at_root_and_nested_depths() {
        use crate::compiler::ast::expression_node::ExpressionNode;
        use lpc_rs_core::lpc_type::LpcType;

        let cast = || CastNode {
            type_: LpcType::Int(false).into(),
            expr: Box::new(VarNode::new("value").into()),
            span: None,
        };
        let mut node = cast();
        let mut printer = TreePrinter::new();
        printer.visit_cast(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Cast",
                "  type: int",
                "  expr: ",
                "    Var",
                "      name: value",
                "Int: 9"
            ]
        );

        let mut node = ArrayNode::new(vec![ExpressionNode::Cast(cast())]);
        let mut printer = TreePrinter::new();
        printer.visit_array(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Array ({",
                "  Cast",
                "    type: int",
                "    expr: ",
                "      Var",
                "        name: value",
                "})",
                "Int: 9"
            ]
        );
    }

    #[tokio::test]
    async fn while_loops_render_condition_body_and_restore_indentation() {
        use crate::compiler::ast::ast_node::AstNode;

        let mut node = WhileNode::new(
            IntNode::new(1).into(),
            AstNode::Return(ReturnNode::new(Some(IntNode::new(7).into()))),
            None,
        );
        let mut printer = TreePrinter::new();
        printer.visit_while(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "While",
                "  Int: 1",
                "body:",
                "  Return",
                "    Int: 7",
                "Int: 9"
            ]
        );
    }

    #[tokio::test]
    async fn standalone_class_definitions_render_visibility_fields_and_restore_indentation() {
        use crate::compiler::ast::ast_node::AstNode;

        let mut tree = parsed("private class Record { int count; string label; }").await;
        let AstNode::ClassDef(mut node) = tree.body.remove(0) else {
            panic!("expected a class definition")
        };
        let mut printer = TreePrinter::new();
        printer.visit_class_def(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Class Def: Record",
                "  visibility: private",
                "  field: int count",
                "  field: string label",
                "Int: 9"
            ]
        );
    }

    #[tokio::test]
    async fn class_constructors_render_named_values_at_root_and_nested_depths() {
        use crate::compiler::ast::{ast_node::AstNode, expression_node::ExpressionNode};

        let tree = parsed("class Record { int count; int other; } mixed f() { return new(class Record, count: 7, other: 8); }").await;
        let mut function = tree
            .body
            .into_iter()
            .find_map(|node| match node {
                AstNode::FunctionDef(function) => Some(function),
                _ => None,
            })
            .unwrap();
        let AstNode::Return(mut returned) = function.body.remove(0) else {
            panic!("expected a return")
        };
        let Some(ExpressionNode::ClassInit(mut node)) = returned.value.take() else {
            panic!("expected a class constructor")
        };
        let nested = node.clone();
        let mut printer = TreePrinter::new();
        printer.visit_class_init(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Class Init: class Record",
                "  field: count",
                "    Int: 7",
                "  field: other",
                "    Int: 8",
                "Int: 9"
            ]
        );

        let mut node = ArrayNode::new(vec![ExpressionNode::ClassInit(nested)]);
        let mut printer = TreePrinter::new();
        printer.visit_array(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Array ({",
                "  Class Init: class Record",
                "    field: count",
                "      Int: 7",
                "    field: other",
                "      Int: 8",
                "})",
                "Int: 9"
            ]
        );
    }

    #[tokio::test]
    async fn member_chains_and_compound_values_render_nested_receivers() {
        use crate::compiler::ast::{
            binary_op_node::BinaryOperation, expression_node::ExpressionNode,
        };
        use lpc_rs_core::call_namespace::CallNamespace;
        use ustr::ustr;

        let receiver = MemberNode {
            receiver: Box::new(VarNode::new("record").into()),
            name: ustr("next"),
            span: None,
        };
        let member = MemberNode {
            receiver: Box::new(ExpressionNode::Member(receiver)),
            name: ustr("count"),
            span: None,
        };
        let mut read = member.clone();
        let compound = CompoundAssignmentNode {
            lhs: Box::new(ExpressionNode::Member(member)),
            rhs: Box::new(IntNode::new(7).into()),
            op: BinaryOperation::Add,
            span: None,
        };
        let mut call = CallNode {
            chain: CallChain::Root {
                receiver: None,
                namespace: CallNamespace::Local,
                name: ustr("consume"),
            },
            arguments: vec![ExpressionNode::CompoundAssignment(compound)],
            span: None,
            lvalue_temps: vec![],
        };
        let mut printer = TreePrinter::new();
        printer.visit_call(&mut call).await.unwrap();
        printer.visit_member(&mut read).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Call",
                "  name: consume",
                "  args:",
                "    Compound Assignment: +",
                "      lhs:",
                "        Member: count",
                "          Member: next",
                "            Var",
                "              name: record",
                "      rhs:",
                "        Int: 7",
                "Member: count",
                "  Member: next",
                "    Var",
                "      name: record",
                "Int: 9",
            ]
        );
    }

    #[tokio::test]
    async fn captures_render_initialization_values_and_restore_indentation() {
        use lpc_rs_core::lpc_type::LpcType;
        use ustr::ustr;

        let mut initializer = VarInitNode::new("saved", LpcType::Mixed(false));
        initializer.value = Some(IntNode::new(7).into());
        let mut capture = CaptureNode {
            name: ustr("saved"),
            expr: Box::new(IntNode::new(7).into()),
            span: None,
        };
        let mut printer = TreePrinter::new();
        printer
            .visit_capture_initializer(&mut initializer)
            .await
            .unwrap();
        printer.visit_capture(&mut capture).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();

        assert_eq!(
            printer.into_lines(),
            [
                "Capture Init",
                "  VarInit",
                "    name: saved",
                "    type: mixed",
                "    value:",
                "      Int: 7",
                "    array: false",
                "Capture: saved",
                "  Int: 7",
                "Int: 9",
            ]
        );
    }

    #[tokio::test]
    async fn spreads_render_their_operand_at_root_and_inside_arrays() {
        use crate::compiler::ast::expression_node::ExpressionNode;

        let spread = || SpreadNode {
            expr: Box::new(VarNode::new("items").into()),
            span: None,
        };
        let mut node = spread();
        let mut printer = TreePrinter::new();
        printer.visit_spread(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            ["Spread", "  Var", "    name: items", "Int: 9"]
        );

        let mut node = ArrayNode::new(vec![ExpressionNode::Spread(spread())]);
        let mut printer = TreePrinter::new();
        printer.visit_array(&mut node).await.unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(
            printer.into_lines(),
            [
                "Array ({",
                "  Spread",
                "    Var",
                "      name: items",
                "})",
                "Int: 9"
            ]
        );
    }

    #[tokio::test]
    async fn nested_fragments_preserve_their_relative_indentation() {
        use crate::compiler::ast::{
            ast_node::AstNode, binary_op_node::BinaryOperation, expression_node::ExpressionNode,
            operator_node::Operator,
        };
        use lpc_rs_core::call_namespace::CallNamespace;
        use ustr::ustr;

        fn root_call(
            receiver: Option<ExpressionNode>,
            name: &str,
            arguments: Vec<ExpressionNode>,
        ) -> CallNode {
            CallNode {
                chain: CallChain::Root {
                    receiver: receiver.map(Box::new),
                    namespace: CallNamespace::Local,
                    name: ustr(name),
                },
                arguments,
                span: None,
                lvalue_temps: vec![],
            }
        }

        let factory = root_call(None, "factory", vec![]);
        let fragments = [
            AstNode::For(ForNode::new(
                Some(AstNode::Expression(IntNode::new(1).into())),
                Some(IntNode::new(2).into()),
                Some(IntNode::new(3).into()),
                AstNode::Block(BlockNode::new(vec![])),
                None,
            )),
            AstNode::Call(root_call(
                Some(VarNode::new("target").into()),
                "invoke",
                vec![IntNode::new(7).into()],
            )),
            AstNode::Call(CallNode {
                chain: CallChain::Node(Box::new(ExpressionNode::Call(factory))),
                arguments: vec![IntNode::new(7).into()],
                span: None,
                lvalue_temps: vec![],
            }),
            AstNode::Expression(ExpressionNode::FunctionPtr(FunctionPtrNode {
                receiver: Some(FunctionPtrReceiver::Static(Box::new(
                    VarNode::new("target").into(),
                ))),
                arguments: Some(vec![Some(IntNode::new(7).into()), None]),
                name: ustr("invoke"),
                span: None,
            })),
            AstNode::Expression(ExpressionNode::Operator(OperatorNode::new(
                Operator::Binary(BinaryOperation::Add),
                Some(vec![Some(IntNode::new(7).into()), None]),
                ustr("addition"),
                None,
            ))),
        ];
        for (index, fragment) in fragments.into_iter().enumerate() {
            let mut root = fragment.clone();
            let mut printer = TreePrinter::new();
            root.visit(&mut printer).await.unwrap();
            let lines = printer.into_lines();

            let mut nested = BlockNode::new(vec![fragment]);
            let mut printer = TreePrinter::new();
            nested.visit(&mut printer).await.unwrap();
            printer.visit_int(&mut IntNode::new(9)).await.unwrap();
            let mut expected = vec!["Block {".to_owned()];
            expected.extend(lines.into_iter().map(|line| format!("  {line}")));
            expected.extend(["}".to_owned(), "Int: 9".to_owned()]);
            assert_eq!(printer.into_lines(), expected, "fragment {index}");
        }
    }

    #[tokio::test]
    async fn float_literals_keep_their_value_type_and_signed_zero() {
        let mut printer = TreePrinter::new();
        printer.visit_float(&mut FloatNode::new(1.5)).await.unwrap();
        printer
            .visit_float(&mut FloatNode::new(-0.0))
            .await
            .unwrap();
        printer.visit_int(&mut IntNode::new(9)).await.unwrap();
        assert_eq!(printer.into_lines(), ["Float: 1.5", "Float: -0", "Int: 9"]);
    }
}
