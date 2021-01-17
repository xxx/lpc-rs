use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::call_node::CallNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::function_def_node::FunctionDefNode;

pub trait TreeWalker {
    fn visit_program(&mut self, node: &ProgramNode) where Self: Sized {
        for expr in &node.functions {
            expr.visit(self);
        }
    }

    fn visit_call(&mut self, node: &CallNode) where Self: Sized {
        for argument in &node.arguments {
            argument.visit(self);
        }
    }

    fn visit_int(&mut self, _node: &IntNode) {}

    fn visit_binary_op(&mut self, node: &BinaryOpNode) where Self: Sized {
        node.l.visit(self);
        node.r.visit(self);
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) where Self: Sized {
        for expression in &node.body {
            expression.visit(self);
        }
    }
}
