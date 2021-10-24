use crate::{
    ast::{
        array_node::ArrayNode, assignment_node::AssignmentNode, ast_node::AstNodeTrait,
        binary_op_node::BinaryOpNode, block_node::BlockNode, break_node::BreakNode,
        call_node::CallNode, comma_expression_node::CommaExpressionNode,
        continue_node::ContinueNode, decl_node::DeclNode, do_while_node::DoWhileNode,
        float_node::FloatNode, for_node::ForNode, function_def_node::FunctionDefNode,
        function_ptr_node::FunctionPtrNode, if_node::IfNode, int_node::IntNode,
        label_node::LabelNode, mapping_node::MappingNode, program_node::ProgramNode,
        range_node::RangeNode, return_node::ReturnNode, string_node::StringNode,
        switch_node::SwitchNode, ternary_node::TernaryNode, unary_op_node::UnaryOpNode,
        var_init_node::VarInitNode, var_node::VarNode, while_node::WhileNode,
    },
    context::Context,
    Result,
};

pub trait ContextHolder {
    /// Consume this walker, and return its `Context`.
    ///
    /// This is intended for use after a walker has completed processing, and
    /// you're ready to re-take ownership of the context for the next step.
    fn into_context(self) -> Context;
}

/// A trait for types that can walk abstract syntax trees
pub trait TreeWalker {
    /// Visit an array literal node
    fn visit_array(&mut self, node: &mut ArrayNode) -> Result<()>
    where
        Self: Sized,
    {
        for node in &mut node.value {
            node.visit(self)?;
        }

        Ok(())
    }

    /// Visit an assignment node
    fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()>
    where
        Self: Sized,
    {
        node.lhs.visit(self)?;
        node.rhs.visit(self)?;

        Ok(())
    }

    /// Visit a binary operation node
    fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()>
    where
        Self: Sized,
    {
        node.l.visit(self)?;
        node.r.visit(self)?;

        Ok(())
    }

    /// Visit a code block
    fn visit_block(&mut self, node: &mut BlockNode) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.body {
            expr.visit(self)?;
        }

        Ok(())
    }

    /// Visit a break node
    fn visit_break(&mut self, _node: &mut BreakNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a function call node
    fn visit_call(&mut self, node: &mut CallNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(rcvr) = &mut node.receiver {
            rcvr.visit(self)?;
        }

        for argument in &mut node.arguments {
            argument.visit(self)?;
        }

        Ok(())
    }

    /// Visit a comma expression
    fn visit_comma_expression(&mut self, node: &mut CommaExpressionNode) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.value {
            let _ = expr.visit(self);
        }

        Ok(())
    }

    /// Visit a continue node
    fn visit_continue(&mut self, _node: &mut ContinueNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a variable declaration node
    fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()>
    where
        Self: Sized,
    {
        for init in &mut node.initializations {
            init.visit(self)?;
        }

        Ok(())
    }

    /// Visit a `do {} while` loop
    fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.body.visit(self);
        let _ = node.condition.visit(self);

        Ok(())
    }

    /// Visit a float (literal) node
    fn visit_float(&mut self, _node: &mut FloatNode) -> Result<()> {
        Ok(())
    }

    /// Visit a `for` loop
    fn visit_for(&mut self, node: &mut ForNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(n) = &mut *node.initializer {
            let _ = n.visit(self);
        }
        if let Some(n) = &mut node.condition {
            let _ = n.visit(self);
        }

        let _ = node.body.visit(self);

        if let Some(n) = &mut node.incrementer {
            let _ = n.visit(self);
        }

        Ok(())
    }

    /// Visit a function definition node
    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()>
    where
        Self: Sized,
    {
        for parameter in &mut node.parameters {
            parameter.visit(self)?;
        }

        for expression in &mut node.body {
            expression.visit(self)?;
        }

        Ok(())
    }

    /// Visit a function pointer node
    fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(rcvr) = &mut node.receiver {
            rcvr.visit(self)?;
        }

        if let Some(args) = &mut node.arguments {
            for argument in args.iter_mut().flatten() {
                argument.visit(self)?;
            }
        }

        Ok(())
    }

    /// Visit an `if` statement
    fn visit_if(&mut self, node: &mut IfNode) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self);
        let _ = node.body.visit(self);
        if let Some(n) = &mut *node.else_clause {
            let _ = n.visit(self);
        }

        Ok(())
    }

    /// Visit an int (literal) node
    fn visit_int(&mut self, _node: &mut IntNode) -> Result<()> {
        Ok(())
    }

    /// Visit a case label
    fn visit_label(&mut self, node: &mut LabelNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut node.case {
            expr.visit(self)?;
        }

        Ok(())
    }

    /// Visit a mapping literal node
    fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()>
    where
        Self: Sized,
    {
        for (key, value) in &mut node.value {
            key.visit(self)?;
            value.visit(self)?;
        }

        Ok(())
    }

    /// Visit a program node. This is the top-level translation unit.
    fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.body {
            expr.visit(self)?;
        }

        Ok(())
    }

    /// Visit a range literal
    fn visit_range(&mut self, node: &mut RangeNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut *node.l {
            expr.visit(self)?;
        }

        if let Some(expr) = &mut *node.r {
            expr.visit(self)?;
        }

        Ok(())
    }

    /// Visit a function return node
    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
        }

        Ok(())
    }

    /// Visit a string (literal) node
    fn visit_string(&mut self, _node: &mut StringNode) -> Result<()> {
        Ok(())
    }

    /// Visit a `switch` statement
    fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()>
    where
        Self: Sized,
    {
        node.expression.visit(self)?;
        node.body.visit(self)
    }

    /// Visit a ternary expression
    fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self);
        let _ = node.body.visit(self);
        let _ = node.else_clause.visit(self);

        Ok(())
    }

    /// Visit a binary operation node
    fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()>
    where
        Self: Sized,
    {
        node.expr.visit(self)?;

        Ok(())
    }

    /// Visit a variable use node
    fn visit_var(&mut self, _node: &mut VarNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a variable initialization node
    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut node.value {
            expr.visit(self)?;
        }

        Ok(())
    }

    /// Visit a `while` loop
    fn visit_while(&mut self, node: &mut WhileNode) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self);
        let _ = node.body.visit(self);

        Ok(())
    }
}
