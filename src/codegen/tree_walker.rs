use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::call_node::CallNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::function_def_node::FunctionDefNode;
use crate::ast::return_node::ReturnNode;
use crate::ast::decl_node::DeclNode;
use crate::ast::var_init_node::VarInitNode;
use crate::ast::var_node::VarNode;
use crate::ast::assignment_node::AssignmentNode;
use crate::ast::string_node::StringNode;

/// A trait for types that can walk abstract syntax trees
pub trait TreeWalker {
    /// Visit a program node. This is the top-level translation unit.
    fn visit_program(&mut self, node: &ProgramNode) where Self: Sized {
        for expr in &node.functions {
            expr.visit(self);
        }
    }

    /// Visit a function call node
    fn visit_call(&mut self, node: &CallNode) where Self: Sized {
        for argument in &node.arguments {
            argument.visit(self);
        }
    }

    /// Visit an int (literal) node
    fn visit_int(&mut self, _node: &IntNode) {}

    /// Visit a string (literal) node
    fn visit_string(&mut self, _node: &StringNode) {}

    /// Visit a binary operation node
    fn visit_binary_op(&mut self, node: &BinaryOpNode) where Self: Sized {
        node.l.visit(self);
        node.r.visit(self);
    }

    /// Visit a function definition node
    fn visit_function_def(&mut self, node: &FunctionDefNode) where Self: Sized {
        for parameter in &node.parameters {
            parameter.visit(self);
        }

        for expression in &node.body {
            expression.visit(self);
        }
    }

    /// Visit a function return node
    fn visit_return(&mut self, node: &ReturnNode) where Self: Sized {
        if let Some(expression) = &node.value {
            expression.visit(self);
        }
    }

    /// Visit a variable declaration node
    fn visit_decl(&mut self, node: &DeclNode) where Self: Sized {
        for init in &node.initializations {
            init.visit(self);
        }
    }

    /// Visit a variable initialization node
    fn visit_var_init(&mut self, node: &VarInitNode) where Self: Sized {
        if let Some(expr) = &node.value {
            expr.visit(self);
        }
    }

    /// Visit a variable use node
    fn visit_var(&mut self, _node: &VarNode) where Self: Sized {}

    /// Visit an assignment node
    fn visit_assignment(&mut self, node: &AssignmentNode) where Self: Sized {
        node.lhs.visit(self);
        node.rhs.visit(self);
    }
}
