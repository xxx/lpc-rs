use std::collections::HashMap;
use crate::semantic::scope_tree::ScopeTree;
use crate::codegen::tree_walker::TreeWalker;
use crate::errors::compiler_error::CompilerError;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::semantic::semantic_checks::{check_binary_operation_types, node_type};
use crate::ast::assignment_node::AssignmentNode;
use crate::errors::compiler_error::assignment_error::AssignmentError;
use crate::ast::int_node::IntNode;
use crate::ast::expression_node::ExpressionNode;
use crate::semantic::function_prototype::FunctionPrototype;
use crate::ast::call_node::CallNode;
use crate::interpreter::efun::{EFUNS, EFUN_PROTOTYPES};
use crate::errors::compiler_error::unknown_function_error::UnknownFunctionError;
use crate::errors::compiler_error::arg_type_error::ArgTypeError;
use crate::errors::compiler_error::return_type_error::ReturnTypeError;
use crate::semantic::lpc_type::LPCType;
use crate::ast::var_init_node::VarInitNode;
use crate::ast::return_node::ReturnNode;
use crate::ast::function_def_node::FunctionDefNode;
use crate::errors::compiler_error::arg_count_error::ArgCountError;

/// A tree walker to handle various semantic & type checks
pub struct SemanticCheckWalker<'a> {
    /// The collection of scopes, to resolve var types
    pub scopes: &'a ScopeTree,

    /// The map of function names, to their respective prototypes.
    /// Used for checking forward references.
    pub function_prototypes: &'a HashMap<String, FunctionPrototype>,

    /// The errors we collect as we traverse the tree
    errors: Vec<CompilerError>,

    /// Track the current function, so we can type check returns.
    current_function: Option<FunctionDefNode>,
}

impl<'a> SemanticCheckWalker<'a> {
    pub fn new(scopes: &'a ScopeTree,
               function_prototypes: &'a HashMap<String, FunctionPrototype>) -> Self {
        Self {
            scopes,
            function_prototypes,
            errors: vec![],
            current_function: None
        }
    }

    /// A transformation helper to get a map of function names to their return values.
    fn function_return_values(&self) -> HashMap<&str, LPCType> {
        self
            .function_prototypes
            .keys()
            .map(|k| k.as_str())
            .zip(
                self.function_prototypes.values().map(|v| v.return_type)
            ).collect::<HashMap<_, _>>()
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

        // Check function existence.
        if !self.function_prototypes.contains_key(&node.name) && !EFUNS.contains_key(node.name.as_str()) {
            let e = CompilerError::UnknownFunctionError(UnknownFunctionError {
                name: node.name.clone(),
                span: node.span
            });
            self.errors.push(e);
            // Non-fatal. Continue.
        }

        // Further checks require access to the function prototype for error messaging.
        let proto_opt = if let Some(prototype) = self.function_prototypes.get(&node.name) {
            Some(prototype)
        } else if let Some(prototype) = EFUN_PROTOTYPES.get(node.name.as_str()) {
            Some(prototype)
        } else {
            None
        };

        if let Some(prototype) = proto_opt {
            let arg_len = node.arguments.len();

            // Check function arity.
            if !((prototype.num_args - prototype.num_default_args)..=prototype.num_args).contains(&arg_len) {
                let e = CompilerError::ArgCountError(ArgCountError {
                    name: node.name.clone(),
                    expected: prototype.num_args,
                    actual: arg_len,
                    span: node.span,
                    prototype_span: prototype.span
                });
                self.errors.push(e);
            }

            // Check argument types.
            for (index, ty) in prototype.arg_types.iter().enumerate() {
                if let Some(arg) = node.arguments.get(index) {
                    // Literal zero is always allowed
                    if let ExpressionNode::Int(IntNode { value: 0, .. }) = *arg {
                        // sigh.
                    } else {
                        let arg_type = node_type(
                            arg,
                            self.scopes,
                            &self.function_return_values()
                        );
                        if !ty.matches_type(arg_type) {
                            self.errors.push(CompilerError::ArgTypeError(ArgTypeError {
                                name: node.name.clone(),
                                type_: arg_type,
                                expected: *ty,
                                span: arg.span(),
                                declaration_span: if let Some(span) = prototype.arg_spans.get(index) {
                                    Some(*span)
                                } else {
                                    None
                                }
                            }));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) -> Result<(), CompilerError> {
        node.l.visit(self)?;
        node.r.visit(self)?;

        match check_binary_operation_types(node, self.scopes, &self.function_return_values()) {
            Ok(_) => Ok(()),
            Err(err) => {
                let e = CompilerError::BinaryOperationError(err);
                self.errors.push(e.clone());
                Err(e)
            }
        }
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) -> Result<(), CompilerError> {
        self.current_function = Some(node.clone());

        for parameter in &node.parameters {
            parameter.visit(self)?;
        }

        for expression in &node.body {
            expression.visit(self)?;
        }

        Ok(())
    }

    fn visit_return(&mut self, node: &ReturnNode) -> Result<(), CompilerError> {
        if let Some(expression) = &node.value {
            expression.visit(self)?;
        }

        if let Some(function_def) = &self.current_function {
            if let Some(expression) = &node.value {
                if let ExpressionNode::Int(IntNode { value: 0, .. }) = expression {
                    // returning a literal 0 is allowable for any type, including void.
                } else {
                    let return_type = node_type(
                        expression,
                        self.scopes,
                        &self.function_return_values()
                    );

                    if function_def.return_type == LPCType::Void ||
                        !function_def.return_type.matches_type(return_type) {
                        let error = CompilerError::ReturnTypeError(ReturnTypeError {
                            type_: return_type,
                            expected: function_def.return_type,
                            span: node.span
                        });

                        self.errors.push(error);
                    }
                }
            } else if function_def.return_type != LPCType::Void {
                let error = CompilerError::ReturnTypeError(ReturnTypeError {
                    type_: LPCType::Void,
                    expected: function_def.return_type,
                    span: node.span
                });

                self.errors.push(error);
            }
        } // else warn?

        Ok(())
    }

    fn visit_var_init(&mut self, node: &VarInitNode) -> Result<(), CompilerError> {
        if let Some(expression) = &node.value {
            expression.visit(self)?;

            let expr_type = node_type(
                expression,
                self.scopes,
                &self.function_return_values()
            );

            let ret = if node.type_.matches_type(expr_type) {
                Ok(())
            } else if let ExpressionNode::Int(IntNode { value: 0, .. }) = *expression {
                // The integer 0 is always a valid assignment.
                Ok(())
            } else {
                let e = CompilerError::AssignmentError(AssignmentError {
                    left_name: node.name.to_string(),
                    left_type: node.type_,
                    right_name: expression.to_string(),
                    right_type: expr_type,
                    span: node.span
                });

                self.errors.push(e.clone());

                Err(e)
            };

            return ret;
        }

        Ok(())
    }

    fn visit_assignment(&mut self, node: &AssignmentNode) -> Result<(), CompilerError> {
        node.lhs.visit(self)?;
        node.rhs.visit(self)?;

        println!("ass: {:?}", node.lhs);
        let left_type = node_type(&node.lhs, self.scopes, &self.function_return_values());
        let right_type = node_type(&node.rhs, self.scopes, &self.function_return_values());

        if left_type.matches_type(right_type) {
            Ok(())
        } else if let ExpressionNode::Int(IntNode { value: 0, .. }) = *node.rhs {
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
    use crate::semantic::lpc_type::LPCType;
    use std::borrow::BorrowMut;
    use crate::ast::var_node::VarNode;

    mod test_visit_call {
        use super::*;

        #[test]
        fn test_visit_call_allows_known_functions() {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "known".to_string(),
                span: None
            });

            let mut functions = HashMap::new();
            functions.insert(String::from("known"), FunctionPrototype {
                name: String::from("known"),
                return_type: LPCType::Int(false),
                num_args: 0,
                num_default_args: 0,
                arg_types: vec![],
                span: None,
                arg_spans: vec![]
            });

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_call_allows_known_efuns() {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
                name: "dump".to_string(),
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());

            assert!(walker.errors.is_empty());
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
            let _ = node.visit(walker.borrow_mut());
            assert_eq!(walker.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_disallows_incorrect_function_arity() {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "print".to_string(),
                span: None
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());
            assert_eq!(walker.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_understands_argument_defaults() {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "my_func".to_string(),
                span: None
            });

            let mut functions = HashMap::new();
            functions.insert(String::from("my_func"), FunctionPrototype {
                name: String::from("my_func"),
                return_type: LPCType::Int(false),
                num_args: 1,
                num_default_args: 1,
                arg_types: vec![LPCType::String(false)],
                span: None,
                arg_spans: vec![]
            });

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());
            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_call_disallows_invalid_arg_types() {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(123)],
                name: "my_func".to_string(),
                span: None
            });

            let mut functions = HashMap::new();
            functions.insert(String::from("my_func"), FunctionPrototype {
                name: String::from("my_func"),
                return_type: LPCType::Int(false),
                num_args: 1,
                num_default_args: 0,
                arg_types: vec![LPCType::String(false)],
                span: None,
                arg_spans: vec![]
            });

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());
            assert_eq!(walker.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_allows_0() {
            let node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(0)],
                name: "my_func".to_string(),
                span: None
            });

            let mut functions = HashMap::new();
            functions.insert(String::from("my_func"), FunctionPrototype {
                name: String::from("my_func"),
                return_type: LPCType::String(false),
                num_args: 1,
                num_default_args: 0,
                arg_types: vec![LPCType::String(false)],
                span: None,
                arg_spans: vec![]
            });

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());
            assert!(walker.errors.is_empty());
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
            let sym = Symbol::new("foo", LPCType::Int(false));
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
            let sym = Symbol::new("foo", LPCType::String(false));
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
            let sym = Symbol::new("foo", LPCType::Int(false));
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
            let sym = Symbol::new("foo", LPCType::String(false));
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
            let sym = Symbol::new("foo", LPCType::String(false));
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            assert!(node.visit(walker.borrow_mut()).is_err());
        }
    }

    mod test_visit_var_init {
        use super::*;

        #[test]
        fn test_visit_var_init_validates_both_sides() {
            let node = VarInitNode {
                name: "foo".to_string(),
                type_: LPCType::Int(false),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_assignment_always_allows_0() {
            let node = VarInitNode {
                type_: LPCType::String(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(0)),
                array: false,
                global: false,
                span: None
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_assignment_disallows_differing_types() {
            let node = VarInitNode {
                type_: LPCType::String(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(walker.borrow_mut());

            assert!(!walker.errors.is_empty());
        }
    }

    mod test_visit_return {
        use super::*;

        mod test_visit_return {
            use super::*;

            #[test]
            fn test_visit_return() {
                let void_node = ReturnNode {
                    value: None, // indicates a Void return value.
                    span: None
                };

                let int_node = ReturnNode {
                    value: Some(ExpressionNode::from(100)),
                    span: None
                };

                let void_function_def = FunctionDefNode {
                    return_type: LPCType::Void,
                    name: "foo".to_string(),
                    parameters: vec![],
                    body: vec![],
                    span: None
                };

                let int_function_def = FunctionDefNode {
                    return_type: LPCType::Int(false),
                    name: "snuh".to_string(),
                    parameters: vec![],
                    body: vec![],
                    span: None
                };

                let functions = HashMap::new();
                let mut scope_tree = ScopeTree::default();
                scope_tree.push_new();
                let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);

                // return void from void function
                walker.current_function = Some(void_function_def.clone());
                let _ = void_node.visit(walker.borrow_mut());
                assert!(walker.errors.is_empty());

                // return void from non-void function
                walker.current_function = Some(int_function_def);
                let _ = void_node.visit(walker.borrow_mut());
                assert!(!walker.errors.is_empty());

                walker.errors = vec![];

                // return int from int function
                let _ = int_node.visit(walker.borrow_mut());
                assert!(walker.errors.is_empty());

                // return int from void function
                walker.current_function = Some(void_function_def);
                let _ = int_node.visit(walker.borrow_mut());
                assert!(!walker.errors.is_empty());
            }

            #[test]
            fn test_visit_return_allows_0() {
                let node = ReturnNode {
                    value: Some(ExpressionNode::from(0)),
                    span: None
                };

                let void_function_def = FunctionDefNode {
                    return_type: LPCType::Void,
                    name: "foo".to_string(),
                    parameters: vec![],
                    body: vec![],
                    span: None
                };

                let functions = HashMap::new();
                let mut scope_tree = ScopeTree::default();
                scope_tree.push_new();
                let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
                walker.current_function = Some(void_function_def);
                let _ = node.visit(walker.borrow_mut());

                assert!(walker.errors.is_empty());
            }

            #[test]
            fn test_visit_return_allows_mixed() {
                let node = ReturnNode {
                    value: Some(ExpressionNode::from(123)),
                    span: None
                };

                let function_def = FunctionDefNode {
                    return_type: LPCType::Mixed(false),
                    name: "foo".to_string(),
                    parameters: vec![],
                    body: vec![],
                    span: None
                };

                let functions = HashMap::new();
                let mut scope_tree = ScopeTree::default();
                scope_tree.push_new();
                let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
                walker.current_function = Some(function_def);
                let _ = node.visit(walker.borrow_mut());

                println!("{:?}", walker.errors);

                assert!(walker.errors.is_empty());
            }
        }

        mod test_visit_assignment {
            use super::*;

            #[test]
            fn test_visit_assignment_always_allows_0() {
                // It's easiest to test visit_assignment() through VarInitNodes
                let node = VarInitNode {
                    type_: LPCType::String(false),
                    name: "foo".to_string(),
                    value: Some(ExpressionNode::from(0)),
                    array: false,
                    global: false,
                    span: None
                };

                let functions = HashMap::new();
                let mut scope_tree = ScopeTree::default();
                scope_tree.push_new();
                let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
                let _ = node.visit(walker.borrow_mut());

                assert!(walker.errors.is_empty());
            }

            #[test]
            fn test_visit_assignment_allows_mixed() {
                let init_node = VarInitNode {
                    type_: LPCType::Mixed(false),
                    name: "foo".to_string(),
                    value: Some(ExpressionNode::from(324)),
                    array: false,
                    global: false,
                    span: None
                };

                let var_node = VarNode::new("foo");

                let assignment_node = AssignmentNode {
                    lhs: Box::new(ExpressionNode::Var(var_node)),
                    rhs: Box::new(ExpressionNode::from("foobar")),
                    op: AssignmentOperation::Simple,
                    span: None
                };

                let functions = HashMap::new();
                let mut scope_tree = ScopeTree::default();
                scope_tree.push_new();
                let sym = Symbol::new("foo", LPCType::Mixed(false));
                scope_tree.get_current_mut().unwrap().insert(sym);

                let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
                let _ = init_node.visit(walker.borrow_mut());

                assert!(walker.errors.is_empty());

                let _ = assignment_node.visit(walker.borrow_mut());

                assert!(walker.errors.is_empty());
            }

            #[test]
            fn test_visit_assignment_disallows_differing_types() {
                let node = VarInitNode {
                    type_: LPCType::String(false),
                    name: "foo".to_string(),
                    value: Some(ExpressionNode::from(123)),
                    array: false,
                    global: false,
                    span: None
                };

                let functions = HashMap::new();
                let mut scope_tree = ScopeTree::default();
                scope_tree.push_new();
                let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
                let _ = node.visit(walker.borrow_mut());

                assert!(!walker.errors.is_empty());
            }
        }
    }
}