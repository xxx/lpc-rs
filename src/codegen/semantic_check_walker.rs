use crate::semantic::scope_tree::ScopeTree;
use crate::codegen::tree_walker::TreeWalker;
use crate::errors::CompilerError;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::semantic::semantic_checks::{check_binary_operation_types, node_type};
use codespan_reporting::diagnostic::Diagnostic;
use crate::ast::assignment_node::AssignmentNode;
use crate::errors::assignment_error::AssignmentError;

/// A tree walker to handle various semantic & type checks
pub struct SemanticCheckWalker<'a> {
    /// The collection of scopes, to resolve var types
    pub scopes: &'a ScopeTree,

    /// The errors we collect as we go through the tree
    errors: Vec<CompilerError>
}

impl<'a> SemanticCheckWalker<'a> {
    pub fn new(scopes: &'a ScopeTree) -> Self {
        Self {
            scopes,
            errors: vec![]
        }
    }
}

impl<'a> TreeWalker for SemanticCheckWalker<'a> {
    fn get_errors(&self) -> Vec<CompilerError> {
        self.errors.to_vec()
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) -> Result<(), CompilerError> {
        node.l.visit(self);
        node.r.visit(self);

        match check_binary_operation_types(node, self.scopes) {
            Ok(_) => Ok(()),
            Err(err) => {
                let e = CompilerError::BinaryOperationError(err);
                self.errors.push(e.clone());
                Err(e)
            }
        }
    }

    fn visit_assignment(&mut self, node: &AssignmentNode) -> Result<(), CompilerError> {
        node.lhs.visit(self);
        node.rhs.visit(self);

        let left_type = node_type(&node.lhs, self.scopes);
        let right_type = node_type(&node.rhs, self.scopes);

        if left_type == right_type {
            Ok(())
        } else {
            let e = CompilerError::AssignmentError(AssignmentError {
                left_name: format!("{}", node.lhs),
                left_type,
                right_name: format!("{}", node.rhs),
                right_type,
                span: node.span
            });

            self.errors.push(e.clone());

            Err(e)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;


}