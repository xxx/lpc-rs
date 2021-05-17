use crate::{
    ast::{
        array_node::ArrayNode, assignment_node::AssignmentNode, ast_node::AstNodeTrait,
        binary_op_node::BinaryOpNode, block_node::BlockNode, call_node::CallNode,
        comma_expression_node::CommaExpressionNode, decl_node::DeclNode, float_node::FloatNode,
        function_def_node::FunctionDefNode, int_node::IntNode, mapping_node::MappingNode,
        program_node::ProgramNode, range_node::RangeNode, return_node::ReturnNode,
        string_node::StringNode, var_init_node::VarInitNode, var_node::VarNode,
    },
    context::Context,
    errors::LpcError,
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
    /// Visit a program node. This is the top-level translation unit.
    fn visit_program(&mut self, node: &mut ProgramNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        for expr in &mut node.body {
            expr.visit(self)?;
        }

        Ok(())
    }

    /// Visit a code block
    fn visit_block(&mut self, node: &mut BlockNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        for expr in &mut node.body {
            expr.visit(self)?;
        }

        Ok(())
    }

    /// Visit a function call node
    fn visit_call(&mut self, node: &mut CallNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        for argument in &mut node.arguments {
            argument.visit(self)?;
        }

        Ok(())
    }

    /// Visit an int (literal) node
    fn visit_int(&mut self, _node: &mut IntNode) -> Result<(), LpcError> {
        Ok(())
    }

    /// Visit a float (literal) node
    fn visit_float(&mut self, _node: &mut FloatNode) -> Result<(), LpcError> {
        Ok(())
    }

    /// Visit a string (literal) node
    fn visit_string(&mut self, _node: &mut StringNode) -> Result<(), LpcError> {
        Ok(())
    }

    /// Visit a binary operation node
    fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        node.l.visit(self)?;
        node.r.visit(self)?;

        Ok(())
    }

    /// Visit a function definition node
    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<(), LpcError>
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

    /// Visit a function return node
    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
        }

        Ok(())
    }

    /// Visit a variable declaration node
    fn visit_decl(&mut self, node: &mut DeclNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        for init in &mut node.initializations {
            init.visit(self)?;
        }

        Ok(())
    }

    /// Visit a variable initialization node
    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut node.value {
            expr.visit(self)?;
        }

        Ok(())
    }

    /// Visit a variable use node
    fn visit_var(&mut self, _node: &mut VarNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit an assignment node
    fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        node.lhs.visit(self)?;
        node.rhs.visit(self)?;

        Ok(())
    }

    /// Visit an array literal node
    fn visit_array(&mut self, node: &mut ArrayNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        for node in &mut node.value {
            node.visit(self)?;
        }

        Ok(())
    }

    /// Visit a mapping literal node
    fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        for (key, value) in &mut node.value {
            key.visit(self)?;
            value.visit(self)?;
        }

        Ok(())
    }

    /// Visit a range literal
    fn visit_range(&mut self, node: &mut RangeNode) -> Result<(), LpcError>
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

    /// Visit a comma expression
    fn visit_comma_expression(&mut self, node: &mut CommaExpressionNode) -> Result<(), LpcError>
    where
        Self: Sized,
    {
        for expr in &mut node.value {
            let _ = expr.visit(self);
        }

        Ok(())
    }
}
