use async_trait::async_trait;
use lpc_rs_errors::Result;

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
        function_ptr_node::{FunctionPtrNode, FunctionPtrReceiver},
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
    compilation_context::CompilationContext,
};

pub trait ContextHolder {
    /// Consume this walker, and return its `Context`.
    ///
    /// This is intended for use after a walker has completed processing, and
    /// you're ready to re-take ownership of the context for the next step.
    fn into_context(self) -> CompilationContext;
}

/// A trait for types that can walk abstract syntax trees
#[async_trait]
pub trait TreeWalker {
    /// Visit an array literal node
    async fn visit_array(&mut self, node: &mut ArrayNode) -> Result<()>
    where
        Self: Sized,
    {
        for node in &mut node.value {
            node.visit(self).await?;
        }

        Ok(())
    }

    /// Visit an assignment node
    async fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()>
    where
        Self: Sized,
    {
        node.lhs.visit(self).await?;
        node.rhs.visit(self).await?;

        Ok(())
    }

    /// Visit a binary operation node
    async fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()>
    where
        Self: Sized,
    {
        node.l.visit(self).await?;
        node.r.visit(self).await?;

        Ok(())
    }

    /// Visit a code block
    async fn visit_block(&mut self, node: &mut BlockNode) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.body {
            expr.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a break node
    async fn visit_break(&mut self, _node: &mut BreakNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a function call node
    async fn visit_call(&mut self, node: &mut CallNode) -> Result<()>
    where
        Self: Sized,
    {
        match &mut node.chain {
            CallChain::Root {
                ref mut receiver, ..
            } => {
                if let Some(rcvr) = receiver {
                    rcvr.visit(self).await?;
                }
            }
            CallChain::Node(ref mut node) => {
                node.visit(self).await?;
            }
        }

        for argument in &mut node.arguments {
            argument.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a closure node
    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(parameters) = &mut node.parameters {
            for param in parameters {
                param.visit(self).await?;
            }
        }

        for expression in &mut node.body {
            expression.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a comma expression
    async fn visit_comma_expression(&mut self, node: &mut CommaExpressionNode) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.value {
            let _ = expr.visit(self).await;
        }

        Ok(())
    }

    /// Visit a continue node
    async fn visit_continue(&mut self, _node: &mut ContinueNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a variable declaration node
    async fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()>
    where
        Self: Sized,
    {
        for init in &mut node.initializations {
            init.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a `do {} while` loop
    async fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.body.visit(self).await;
        let _ = node.condition.visit(self).await;

        Ok(())
    }

    /// Visit a float (literal) node
    async fn visit_float(&mut self, _node: &mut FloatNode) -> Result<()> {
        Ok(())
    }

    /// Visit a `for` loop
    async fn visit_for(&mut self, node: &mut ForNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(n) = &mut *node.initializer {
            let _ = n.visit(self).await;
        }
        if let Some(n) = &mut node.condition {
            let _ = n.visit(self).await;
        }

        let _ = node.body.visit(self).await;

        if let Some(n) = &mut node.incrementer {
            let _ = n.visit(self).await;
        }

        Ok(())
    }

    /// Visit a `foreach` loop
    async fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()>
    where
        Self: Sized,
    {
        match &mut node.initializer {
            ForEachInit::Array(ref mut init) | ForEachInit::String(ref mut init) => {
                let _ = init.visit(self).await;
            }
            ForEachInit::Mapping {
                ref mut key,
                ref mut value,
            } => {
                let _ = key.visit(self).await;
                let _ = value.visit(self).await;
            }
        }
        let _ = node.collection.visit(self).await;
        let _ = node.body.visit(self).await;

        Ok(())
    }

    /// Visit a function definition node
    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()>
    where
        Self: Sized,
    {
        for parameter in &mut node.parameters {
            parameter.visit(self).await?;
        }

        for expression in &mut node.body {
            expression.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a function pointer node
    async fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(FunctionPtrReceiver::Static(rcvr)) = &mut node.receiver {
            rcvr.visit(self).await?;
        }

        if let Some(args) = &mut node.arguments {
            for argument in args.iter_mut().flatten() {
                argument.visit(self).await?;
            }
        }

        Ok(())
    }

    /// Visit an `if` statement
    async fn visit_if(&mut self, node: &mut IfNode) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self).await;
        let _ = node.body.visit(self).await;
        if let Some(n) = &mut *node.else_clause {
            let _ = n.visit(self).await;
        }

        Ok(())
    }

    /// Visit an `inherit` statement
    async fn visit_inherit(&mut self, _node: &mut InheritNode) -> Result<()> {
        Ok(())
    }

    /// Visit an int (literal) node
    async fn visit_int(&mut self, _node: &mut IntNode) -> Result<()> {
        Ok(())
    }

    /// Visit a case label
    async fn visit_label(&mut self, node: &mut LabelNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut node.case {
            expr.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a mapping literal node
    async fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()>
    where
        Self: Sized,
    {
        for (key, value) in &mut node.value {
            key.visit(self).await?;
            value.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a program node. This is the top-level translation unit.
    async fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.inherits {
            expr.visit(self).await?;
        }

        for expr in &mut node.body {
            expr.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a range literal
    async fn visit_range(&mut self, node: &mut RangeNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut *node.l {
            expr.visit(self).await?;
        }

        if let Some(expr) = &mut *node.r {
            expr.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a function return node
    async fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expression) = &mut node.value {
            expression.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a string (literal) node
    async fn visit_string(&mut self, _node: &mut StringNode) -> Result<()> {
        Ok(())
    }

    /// Visit a `switch` statement
    async fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()>
    where
        Self: Sized,
    {
        node.expression.visit(self).await?;
        node.body.visit(self).await
    }

    /// Visit a ternary expression
    async fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self).await;
        let _ = node.body.visit(self).await;
        let _ = node.else_clause.visit(self).await;

        Ok(())
    }

    /// Visit a unary operation node
    async fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()>
    where
        Self: Sized,
    {
        node.expr.visit(self).await?;

        Ok(())
    }

    /// Visit a variable use node
    async fn visit_var(&mut self, _node: &mut VarNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a variable initialization node
    async fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut node.value {
            expr.visit(self).await?;
        }

        Ok(())
    }

    /// Visit a `while` loop
    async fn visit_while(&mut self, node: &mut WhileNode) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self).await;
        let _ = node.body.visit(self).await;

        Ok(())
    }
}
