use std::collections::HashMap;
use crate::semantic::scope_tree::ScopeTree;
use crate::codegen::tree_walker::TreeWalker;
use crate::errors::CompilerError;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::semantic::semantic_checks::{check_binary_operation_types, node_type};
use crate::ast::assignment_node::AssignmentNode;
use crate::errors::assignment_error::AssignmentError;
use crate::ast::int_node::IntNode;
use crate::ast::expression_node::ExpressionNode;
use crate::semantic::function_prototype::FunctionPrototype;
use crate::ast::call_node::CallNode;
use crate::interpreter::efun::EFUNS;
use crate::errors::unknown_function_error::UnknownFunctionError;

/// A tree walker to handle various semantic & type checks
pub struct SemanticCheckWalker<'a> {
    /// The collection of scopes, to resolve var types
    pub scopes: &'a ScopeTree,

    /// The map of function names, to their respective prototypes.
    /// Used for checking forward references.
    pub function_prototypes: &'a HashMap<String, FunctionPrototype>,

    /// The errors we collect as we traverse the tree
    errors: Vec<CompilerError>
}

impl<'a> SemanticCheckWalker<'a> {
    pub fn new(scopes: &'a ScopeTree,
               function_prototypes: &'a HashMap<String, FunctionPrototype>) -> Self {
        Self {
            scopes,
            function_prototypes,
            errors: vec![]
        }
    }
}

impl<'a> TreeWalker for SemanticCheckWalker<'a> {
    fn get_errors(&self) -> Vec<CompilerError> {
        self.errors.to_vec()
    }

    fn visit_call(&mut self, node: &CallNode) -> Result<(), CompilerError> {
        for argument in &node.arguments {
            argument.visit(self)?;
        }

        if !self.function_prototypes.contains_key(&node.name) && !EFUNS.contains_key(&node.name) {
            let e = CompilerError::UnknownFunctionError(UnknownFunctionError {
                name: node.name.clone(),
                span: node.span.clone()
            });
            self.errors.push(e.clone());
            // Non-fatal. Continue.
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) -> Result<(), CompilerError> {
        node.l.visit(self)?;
        node.r.visit(self)?;

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
        node.lhs.visit(self)?;
        node.rhs.visit(self)?;

        let left_type = node_type(&node.lhs, self.scopes);
        let right_type = node_type(&node.rhs, self.scopes);

        if left_type == right_type {
            Ok(())
        } else if let ExpressionNode::Int(IntNode { value: 0 }) = *node.rhs {
            // The integer 0 is always a valid assignment.
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
    use crate::ast::expression_node::ExpressionNode;
    use crate::ast::assignment_node::AssignmentOperation;
    use crate::semantic::symbol::Symbol;
    use crate::semantic::lpc_type::LPCVarType;
    use std::borrow::BorrowMut;
    use crate::ast::var_node::VarNode;

    mod test_visit_call {
        use super::*;

        #[test]
        fn test_visit_call_allows_known_functions() -> Result<(), CompilerError> {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "known".to_string(),
                span: None
            });

            let mut functions = HashMap::new();
            functions.insert(String::from("known"), FunctionPrototype {
                name: String::from("known"),
                num_args: 0,
                arg_types: vec![]
            });

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            node.visit(walker.borrow_mut())
        }

        #[test]
        fn test_visit_call_allows_known_efuns() -> Result<(), CompilerError> {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "print".to_string(),
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            node.visit(walker.borrow_mut())
        }

        #[test]
        fn test_visit_call_disallows_unknown_functions() {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "unknown".to_string(),
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let result = node.visit(walker.borrow_mut());
            assert!(result.is_err());
        }
    }

    mod test_visit_binary_op {
        use super::*;
        use crate::ast::binary_op_node::BinaryOperation;

        #[test]
        fn test_visit_binary_op_validates_both_sides() -> Result<(), CompilerError> {
            let node = ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                r: Box::new(ExpressionNode::from(456)),
                op: BinaryOperation::Add,
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCVarType::Int, false);
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            node.visit(walker.borrow_mut())
        }

        #[test]
        fn test_visit_assignment_disallows_differing_types() {
            let node = ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                r: Box::new(ExpressionNode::from(123)),
                op: BinaryOperation::Sub,
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCVarType::String, false);
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            assert!(node.visit(walker.borrow_mut()).is_err());
        }
    }

    mod test_visit_assignment {
        use super::*;

        #[test]
        fn test_visit_assignment_validates_both_sides() -> Result<(), CompilerError> {
            let node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(456)),
                op: AssignmentOperation::Simple,
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCVarType::Int, false);
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            node.visit(walker.borrow_mut())
        }

        #[test]
        fn test_visit_assignment_always_allows_0() -> Result<(), CompilerError> {
            let node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(0)),
                op: AssignmentOperation::Simple,
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCVarType::String, false);
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            node.visit(walker.borrow_mut())
        }

        #[test]
        fn test_visit_assignment_disallows_differing_types() {
            let node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(123)),
                op: AssignmentOperation::Simple,
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCVarType::String, false);
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            assert!(node.visit(walker.borrow_mut()).is_err());
        }
    }
}