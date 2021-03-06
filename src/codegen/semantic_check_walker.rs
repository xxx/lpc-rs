use crate::{
    ast::{
        assignment_node::AssignmentNode, ast_node::ASTNodeTrait, binary_op_node::BinaryOpNode,
        call_node::CallNode, expression_node::ExpressionNode, function_def_node::FunctionDefNode,
        int_node::IntNode, range_node::RangeNode, return_node::ReturnNode,
        var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::{
        arg_count_error::ArgCountError, arg_type_error::ArgTypeError,
        assignment_error::AssignmentError, return_type_error::ReturnTypeError,
        unknown_function_error::UnknownFunctionError, CompilerError,
    },
    interpreter::efun::{EFUNS, EFUN_PROTOTYPES},
    semantic::{
        function_prototype::FunctionPrototype,
        lpc_type::LPCType,
        scope_tree::ScopeTree,
        semantic_checks::{check_binary_operation_types, node_type},
    },
};
use std::collections::HashMap;

use crate::errors::compiler_error::range_error::RangeError;

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
    pub fn new(
        scopes: &'a ScopeTree,
        function_prototypes: &'a HashMap<String, FunctionPrototype>,
    ) -> Self {
        Self {
            scopes,
            function_prototypes,
            errors: vec![],
            current_function: None,
        }
    }

    /// A transformation helper to get a map of function names to their return values.
    fn function_return_values(&self) -> HashMap<&str, LPCType> {
        self.function_prototypes
            .keys()
            .map(|k| k.as_str())
            .zip(self.function_prototypes.values().map(|v| v.return_type))
            .collect::<HashMap<_, _>>()
    }
}

impl<'a> TreeWalker for SemanticCheckWalker<'a> {
    fn get_errors(&self) -> Vec<CompilerError> {
        self.errors.to_vec()
    }

    fn visit_call(&mut self, node: &mut CallNode) -> Result<(), CompilerError> {
        for argument in &mut node.arguments {
            argument.visit(self)?;
        }

        // Check function existence.
        if !self.function_prototypes.contains_key(&node.name)
            && !EFUNS.contains_key(node.name.as_str())
        {
            let e = CompilerError::UnknownFunctionError(UnknownFunctionError {
                name: node.name.clone(),
                span: node.span.clone(),
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
            if !((prototype.num_args - prototype.num_default_args)..=prototype.num_args)
                .contains(&arg_len)
            {
                let e = CompilerError::ArgCountError(ArgCountError {
                    name: node.name.clone(),
                    expected: prototype.num_args,
                    actual: arg_len,
                    span: node.span.clone(),
                    prototype_span: prototype.span.clone(),
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
                        let arg_type = node_type(arg, self.scopes, &self.function_return_values());
                        if !ty.matches_type(arg_type) {
                            self.errors.push(CompilerError::ArgTypeError(ArgTypeError {
                                name: node.name.clone(),
                                type_: arg_type,
                                expected: *ty,
                                span: arg.span().clone(),
                                declaration_span: if let Some(span) = prototype.arg_spans.get(index)
                                {
                                    Some(span.clone())
                                } else {
                                    None
                                },
                            }));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<(), CompilerError> {
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

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<(), CompilerError> {
        self.current_function = Some(node.clone());

        for parameter in &mut node.parameters {
            parameter.visit(self)?;
        }

        for expression in &mut node.body {
            expression.visit(self)?;
        }

        Ok(())
    }

    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<(), CompilerError> {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
        }

        if let Some(function_def) = &self.current_function {
            if let Some(expression) = &node.value {
                if let ExpressionNode::Int(IntNode { value: 0, .. }) = expression {
                    // returning a literal 0 is allowable for any type, including void.
                } else {
                    let return_type =
                        node_type(expression, self.scopes, &self.function_return_values());

                    if function_def.return_type == LPCType::Void
                        || !function_def.return_type.matches_type(return_type)
                    {
                        let error = CompilerError::ReturnTypeError(ReturnTypeError {
                            type_: return_type,
                            expected: function_def.return_type,
                            span: node.span.clone(),
                        });

                        self.errors.push(error);
                    }
                }
            } else if function_def.return_type != LPCType::Void {
                let error = CompilerError::ReturnTypeError(ReturnTypeError {
                    type_: LPCType::Void,
                    expected: function_def.return_type,
                    span: node.span.clone(),
                });

                self.errors.push(error);
            }
        } // else warn?

        Ok(())
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<(), CompilerError> {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;

            let expr_type = node_type(expression, self.scopes, &self.function_return_values());

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
                    span: node.span.clone(),
                });

                self.errors.push(e.clone());

                Err(e)
            };

            return ret;
        }

        Ok(())
    }

    fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<(), CompilerError> {
        node.lhs.visit(self)?;
        node.rhs.visit(self)?;

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
                span: node.span.clone(),
            });

            self.errors.push(e.clone());

            Err(e)
        }
    }

    fn visit_range(&mut self, node: &mut RangeNode) -> Result<(), CompilerError>
    where
        Self: Sized,
    {
        if let Some(expr) = &mut *node.l {
            expr.visit(self)?;
        }

        if let Some(expr) = &mut *node.r {
            expr.visit(self)?;
        }

        let left_type = if let Some(left) = &*node.l {
            node_type(left, self.scopes, &self.function_return_values())
        } else {
            LPCType::Int(false)
        };

        let right_type = if let Some(right) = &*node.r {
            node_type(right, self.scopes, &self.function_return_values())
        } else {
            LPCType::Int(false)
        };

        // These must resolve to ints at some point.
        let required_types = LPCType::Int(false) | LPCType::Mixed(false);

        if left_type.matches_type(required_types) && right_type.matches_type(required_types) {
            Ok(())
        } else {
            let e = CompilerError::RangeError(RangeError {
                left_name: format!("{:?}", node.l),
                left_type,
                right_name: format!("{:?}", node.r),
                right_type,
                span: node.span.clone(),
            });

            self.errors.push(e.clone());

            Err(e)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{
            assignment_node::AssignmentOperation, expression_node::ExpressionNode,
            var_node::VarNode,
        },
        semantic::{lpc_type::LPCType, symbol::Symbol},
    };

    mod test_visit_call {
        use super::*;

        #[test]
        fn test_visit_call_allows_known_functions() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "known".to_string(),
                span: None,
            });

            let mut functions = HashMap::new();
            functions.insert(
                String::from("known"),
                FunctionPrototype {
                    name: String::from("known"),
                    return_type: LPCType::Int(false),
                    num_args: 0,
                    num_default_args: 0,
                    arg_types: vec![],
                    span: None,
                    arg_spans: vec![],
                },
            );

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_call_allows_known_efuns() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
                name: "dump".to_string(),
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_call_disallows_unknown_functions() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "unknown".to_string(),
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_disallows_incorrect_function_arity() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "print".to_string(),
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_understands_argument_defaults() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "my_func".to_string(),
                span: None,
            });

            let mut functions = HashMap::new();
            functions.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: String::from("my_func"),
                    return_type: LPCType::Int(false),
                    num_args: 1,
                    num_default_args: 1,
                    arg_types: vec![LPCType::String(false)],
                    span: None,
                    arg_spans: vec![],
                },
            );

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);
            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_call_disallows_invalid_arg_types() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(123)],
                name: "my_func".to_string(),
                span: None,
            });

            let mut functions = HashMap::new();
            functions.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: String::from("my_func"),
                    return_type: LPCType::Int(false),
                    num_args: 1,
                    num_default_args: 0,
                    arg_types: vec![LPCType::String(false)],
                    span: None,
                    arg_spans: vec![],
                },
            );

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_allows_0() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(0)],
                name: "my_func".to_string(),
                span: None,
            });

            let mut functions = HashMap::new();
            functions.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: String::from("my_func"),
                    return_type: LPCType::String(false),
                    num_args: 1,
                    num_default_args: 0,
                    arg_types: vec![LPCType::String(false)],
                    span: None,
                    arg_spans: vec![],
                },
            );

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);
            assert!(walker.errors.is_empty());
        }
    }

    mod test_visit_binary_op {
        use super::*;
        use crate::ast::binary_op_node::BinaryOperation;

        #[test]
        fn test_visit_binary_op_validates_both_sides() -> Result<(), CompilerError> {
            let mut node = ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                r: Box::new(ExpressionNode::from(456)),
                op: BinaryOperation::Add,
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::Int(false));
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            node.visit(&mut walker)
        }

        #[test]
        fn test_visit_assignment_disallows_differing_types() {
            let mut node = ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                r: Box::new(ExpressionNode::from(123)),
                op: BinaryOperation::Sub,
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::String(false));
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            assert!(node.visit(&mut walker).is_err());
        }
    }

    mod test_visit_assignment {
        use super::*;
        use crate::ast::binary_op_node::BinaryOperation;

        #[test]
        fn test_visit_assignment_validates_both_sides() -> Result<(), CompilerError> {
            let mut node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(456)),
                op: AssignmentOperation::Simple,
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::Int(false));
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            node.visit(&mut walker)
        }

        #[test]
        fn test_visit_assignment_always_allows_0() -> Result<(), CompilerError> {
            let mut node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(0)),
                op: AssignmentOperation::Simple,
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::String(false));
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            node.visit(&mut walker)
        }

        #[test]
        fn test_visit_assignment_disallows_differing_types() {
            let mut node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(123)),
                op: AssignmentOperation::Simple,
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::String(false));
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            assert!(node.visit(&mut walker).is_err());
        }

        #[test]
        fn test_visit_assignment_allows_mixed() {
            let mut init_node = VarInitNode {
                type_: LPCType::Mixed(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(324)),
                array: false,
                global: false,
                span: None,
            };

            let var_node = VarNode::new("foo");

            let mut assignment_node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(var_node)),
                rhs: Box::new(ExpressionNode::from("foobar")),
                op: AssignmentOperation::Simple,
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::Mixed(false));
            scope_tree.get_current_mut().unwrap().insert(sym);

            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = init_node.visit(&mut walker);

            assert!(walker.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_assignment_allows_array_items() {
            let mut init_node = VarInitNode {
                type_: LPCType::Int(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(324)),
                array: false,
                global: false,
                span: None,
            };

            let var_node = VarNode::new("foo");

            let mut assignment_node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(var_node)),
                rhs: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(vec![1, 2, 3])),
                    r: Box::new(ExpressionNode::from(1)),
                    op: BinaryOperation::Index,
                    span: None,
                })),
                op: AssignmentOperation::Simple,
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::Mixed(false));
            scope_tree.get_current_mut().unwrap().insert(sym);

            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = init_node.visit(&mut walker);

            assert!(walker.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_assignment_allows_array_ranges() {
            let mut init_node = VarInitNode {
                type_: LPCType::Int(true),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(vec![324])),
                array: false,
                global: false,
                span: None,
            };

            let var_node = VarNode::new("foo");

            let mut assignment_node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(var_node)),
                rhs: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(vec![1, 2, 3])),
                    r: Box::new(ExpressionNode::Range(RangeNode {
                        l: Box::new(Some(ExpressionNode::from(1))),
                        r: Box::new(Some(ExpressionNode::from(4))),
                        span: None,
                    })),
                    op: BinaryOperation::Index,
                    span: None,
                })),
                op: AssignmentOperation::Simple,
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::Mixed(false));
            scope_tree.get_current_mut().unwrap().insert(sym);

            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = init_node.visit(&mut walker);

            assert!(walker.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }
    }

    mod test_visit_var_init {
        use super::*;

        #[test]
        fn test_visit_var_init_validates_both_sides() {
            let mut node = VarInitNode {
                name: "foo".to_string(),
                type_: LPCType::Int(false),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_var_init_always_allows_0() {
            let mut node = VarInitNode {
                type_: LPCType::String(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(0)),
                array: false,
                global: false,
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_var_init_disallows_differing_types() {
            let mut node = VarInitNode {
                type_: LPCType::String(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(!walker.errors.is_empty());
        }
    }

    mod test_visit_return {
        use super::*;

        #[test]
        fn test_visit_return() {
            let mut void_node = ReturnNode {
                value: None, // indicates a Void return value.
                span: None,
            };

            let mut int_node = ReturnNode {
                value: Some(ExpressionNode::from(100)),
                span: None,
            };

            let void_function_def = FunctionDefNode {
                return_type: LPCType::Void,
                name: "foo".to_string(),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let int_function_def = FunctionDefNode {
                return_type: LPCType::Int(false),
                name: "snuh".to_string(),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);

            // return void from void function
            walker.current_function = Some(void_function_def.clone());
            let _ = void_node.visit(&mut walker);
            assert!(walker.errors.is_empty());

            // return void from non-void function
            walker.current_function = Some(int_function_def);
            let _ = void_node.visit(&mut walker);
            assert!(!walker.errors.is_empty());

            walker.errors = vec![];

            // return int from int function
            let _ = int_node.visit(&mut walker);
            assert!(walker.errors.is_empty());

            // return int from void function
            walker.current_function = Some(void_function_def);
            let _ = int_node.visit(&mut walker);
            assert!(!walker.errors.is_empty());
        }

        #[test]
        fn test_visit_return_allows_0() {
            let mut node = ReturnNode {
                value: Some(ExpressionNode::from(0)),
                span: None,
            };

            let void_function_def = FunctionDefNode {
                return_type: LPCType::Void,
                name: "foo".to_string(),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            walker.current_function = Some(void_function_def);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_return_allows_mixed() {
            let mut node = ReturnNode {
                value: Some(ExpressionNode::from(123)),
                span: None,
            };

            let function_def = FunctionDefNode {
                return_type: LPCType::Mixed(false),
                name: "foo".to_string(),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            walker.current_function = Some(function_def);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }
    }

    mod test_visit_range {
        use super::*;

        #[test]
        fn test_visit_range_allows_ints() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::Var(VarNode::new("foo")))),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::Int(false));
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_range_disallows_non_ints() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::Var(VarNode::new("foo")))),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let sym = Symbol::new("foo", LPCType::String(false));
            scope_tree.get_current_mut().unwrap().insert(sym);
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(!walker.errors.is_empty());
        }

        #[test]
        fn test_visit_range_allows_start_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(None),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_range_allows_end_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::from(456))),
                r: Box::new(None),
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_visit_range_allows_both_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(None),
                r: Box::new(None),
                span: None,
            });

            let functions = HashMap::new();
            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let mut walker = SemanticCheckWalker::new(&scope_tree, &functions);
            let _ = node.visit(&mut walker);

            assert!(walker.errors.is_empty());
        }
    }
}
