use crate::semantic::scope_tree::ScopeTree;
use crate::codegen::tree_walker::TreeWalker;
use crate::errors::CompilerError;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::semantic::semantic_checks::check_binary_operation_types;
use codespan_reporting::diagnostic::Diagnostic;

/// A tree walker to handle various semantic & type checks
pub struct SemanticCheckWalker<'a> {
    /// The collection of scopes, to resolve var types
    pub scopes: &'a ScopeTree,

    /// The errors we collect as we go through the tree
    pub errors: Vec<CompilerError>
}

impl<'a> SemanticCheckWalker<'a> {
    pub fn new(scopes: &'a ScopeTree) -> Self {
        Self {
            scopes,
            errors: vec![]
        }
    }

    pub fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        self
            .errors
            .iter()
            .flat_map(|e| e.to_diagnostics(file_id))
            .collect()
    }
}

impl<'a> TreeWalker for SemanticCheckWalker<'a> {
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
}