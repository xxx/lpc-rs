use lpc_rs_errors::Result;
use qcell::QCellOwner;

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
pub trait TreeWalker {
    /// Visit an array literal node
    fn visit_array(&mut self, node: &mut ArrayNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        for node in &mut node.value {
            node.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit an assignment node
    fn visit_assignment(
        &mut self,
        node: &mut AssignmentNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()>
    where
        Self: Sized,
    {
        node.lhs.visit(self, cell_key)?;
        node.rhs.visit(self, cell_key)?;

        Ok(())
    }

    /// Visit a binary operation node
    fn visit_binary_op(&mut self, node: &mut BinaryOpNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        node.l.visit(self, cell_key)?;
        node.r.visit(self, cell_key)?;

        Ok(())
    }

    /// Visit a code block
    fn visit_block(&mut self, node: &mut BlockNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.body {
            expr.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a break node
    fn visit_break(&mut self, _node: &mut BreakNode, _cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a function call node
    fn visit_call(&mut self, node: &mut CallNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        match &mut node.chain {
            CallChain::Root {
                ref mut receiver, ..
            } => {
                if let Some(rcvr) = receiver {
                    rcvr.visit(self, cell_key)?;
                }
            }
            CallChain::Node(ref mut node) => {
                node.visit(self, cell_key)?;
            }
        }

        for argument in &mut node.arguments {
            argument.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a closure node
    fn visit_closure(&mut self, node: &mut ClosureNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(parameters) = &mut node.parameters {
            for param in parameters {
                param.visit(self, cell_key)?;
            }
        }

        for expression in &mut node.body {
            expression.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a comma expression
    fn visit_comma_expression(
        &mut self,
        node: &mut CommaExpressionNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.value {
            let _ = expr.visit(self, cell_key);
        }

        Ok(())
    }

    /// Visit a continue node
    fn visit_continue(&mut self, _node: &mut ContinueNode, _cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a variable declaration node
    fn visit_decl(&mut self, node: &mut DeclNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        for init in &mut node.initializations {
            init.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a `do {} while` loop
    fn visit_do_while(&mut self, node: &mut DoWhileNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.body.visit(self, cell_key);
        let _ = node.condition.visit(self, cell_key);

        Ok(())
    }

    /// Visit a float (literal) node
    fn visit_float(&mut self, _node: &mut FloatNode, _cell_key: &mut QCellOwner) -> Result<()> {
        Ok(())
    }

    /// Visit a `for` loop
    fn visit_for(&mut self, node: &mut ForNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(n) = &mut *node.initializer {
            let _ = n.visit(self, cell_key);
        }
        if let Some(n) = &mut node.condition {
            let _ = n.visit(self, cell_key);
        }

        let _ = node.body.visit(self, cell_key);

        if let Some(n) = &mut node.incrementer {
            let _ = n.visit(self, cell_key);
        }

        Ok(())
    }

    /// Visit a `foreach` loop
    fn visit_foreach(&mut self, node: &mut ForEachNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        match &mut node.initializer {
            ForEachInit::Array(ref mut init) | ForEachInit::String(ref mut init) => {
                let _ = init.visit(self, cell_key);
            }
            ForEachInit::Mapping {
                ref mut key,
                ref mut value,
            } => {
                let _ = key.visit(self, cell_key);
                let _ = value.visit(self, cell_key);
            }
        }
        let _ = node.collection.visit(self, cell_key);
        let _ = node.body.visit(self, cell_key);

        Ok(())
    }

    /// Visit a function definition node
    fn visit_function_def(
        &mut self,
        node: &mut FunctionDefNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()>
    where
        Self: Sized,
    {
        for parameter in &mut node.parameters {
            parameter.visit(self, cell_key)?;
        }

        for expression in &mut node.body {
            expression.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a function pointer node
    fn visit_function_ptr(
        &mut self,
        node: &mut FunctionPtrNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(FunctionPtrReceiver::Static(rcvr)) = &mut node.receiver {
            rcvr.visit(self, cell_key)?;
        }

        if let Some(args) = &mut node.arguments {
            for argument in args.iter_mut().flatten() {
                argument.visit(self, cell_key)?;
            }
        }

        Ok(())
    }

    /// Visit an `if` statement
    fn visit_if(&mut self, node: &mut IfNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self, cell_key);
        let _ = node.body.visit(self, cell_key);
        if let Some(n) = &mut *node.else_clause {
            let _ = n.visit(self, cell_key);
        }

        Ok(())
    }

    /// Visit an `inherit` statement
    fn visit_inherit(&mut self, _node: &mut InheritNode, _cell_key: &mut QCellOwner) -> Result<()> {
        Ok(())
    }

    /// Visit an int (literal) node
    fn visit_int(&mut self, _node: &mut IntNode, _cell_key: &mut QCellOwner) -> Result<()> {
        Ok(())
    }

    /// Visit a case label
    fn visit_label(&mut self, node: &mut LabelNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut node.case {
            expr.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a mapping literal node
    fn visit_mapping(&mut self, node: &mut MappingNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        for (key, value) in &mut node.value {
            key.visit(self, cell_key)?;
            value.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a program node. This is the top-level translation unit.
    fn visit_program(&mut self, node: &mut ProgramNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        for expr in &mut node.inherits {
            expr.visit(self, cell_key)?;
        }

        for expr in &mut node.body {
            expr.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a range literal
    fn visit_range(&mut self, node: &mut RangeNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut *node.l {
            expr.visit(self, cell_key)?;
        }

        if let Some(expr) = &mut *node.r {
            expr.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a function return node
    fn visit_return(&mut self, node: &mut ReturnNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expression) = &mut node.value {
            expression.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a string (literal) node
    fn visit_string(&mut self, _node: &mut StringNode, _cell_key: &mut QCellOwner) -> Result<()> {
        Ok(())
    }

    /// Visit a `switch` statement
    fn visit_switch(&mut self, node: &mut SwitchNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        node.expression.visit(self, cell_key)?;
        node.body.visit(self, cell_key)
    }

    /// Visit a ternary expression
    fn visit_ternary(&mut self, node: &mut TernaryNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self, cell_key);
        let _ = node.body.visit(self, cell_key);
        let _ = node.else_clause.visit(self, cell_key);

        Ok(())
    }

    /// Visit a unary operation node
    fn visit_unary_op(&mut self, node: &mut UnaryOpNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        node.expr.visit(self, cell_key)?;

        Ok(())
    }

    /// Visit a variable use node
    fn visit_var(&mut self, _node: &mut VarNode, _cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a variable initialization node
    fn visit_var_init(&mut self, node: &mut VarInitNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut node.value {
            expr.visit(self, cell_key)?;
        }

        Ok(())
    }

    /// Visit a `while` loop
    fn visit_while(&mut self, node: &mut WhileNode, cell_key: &mut QCellOwner) -> Result<()>
    where
        Self: Sized,
    {
        let _ = node.condition.visit(self, cell_key);
        let _ = node.body.visit(self, cell_key);

        Ok(())
    }
}
