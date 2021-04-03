use crate::{
    ast::{
        assignment_node::AssignmentNode, ast_node::AstNodeTrait, binary_op_node::BinaryOpNode,
        call_node::CallNode, expression_node::ExpressionNode, function_def_node::FunctionDefNode,
        int_node::IntNode, range_node::RangeNode, return_node::ReturnNode,
        var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    interpreter::efun::{EFUNS, EFUN_PROTOTYPES},
    semantic::{
        lpc_type::LpcType,
        semantic_checks::{check_binary_operation_types, node_type},
    },
};
use std::collections::HashMap;

use crate::{
    ast::ast_node::SpannedNode, codegen::tree_walker::ContextHolder, context::Context,
    errors::NewError,
};

/// A tree walker to handle various semantic & type checks
pub struct SemanticCheckWalker {
    // /// The collection of scopes, to resolve var types
    // pub scopes: &'a ScopeTree,
    //
    // /// The map of function names, to their respective prototypes.
    // /// Used for checking forward references.
    // pub function_prototypes: &'a HashMap<String, FunctionPrototype>,

    // /// The errors we collect as we traverse the tree
    // errors: Vec<CompilerError>,
    /// Track the current function, so we can type check returns.
    current_function: Option<FunctionDefNode>,

    context: Context,
}

impl SemanticCheckWalker {
    pub fn new(context: Context) -> Self {
        Self {
            context,
            current_function: None,
        }
    }

    /// A transformation helper to get a map of function names to their return values.
    fn function_return_values(&self) -> HashMap<&str, LpcType> {
        self.context
            .function_prototypes
            .keys()
            .map(|k| k.as_str())
            .zip(
                self.context
                    .function_prototypes
                    .values()
                    .map(|v| v.return_type),
            )
            .collect::<HashMap<_, _>>()
    }
}

impl ContextHolder for SemanticCheckWalker {
    fn into_context(self) -> Context {
        self.context
    }
}

impl TreeWalker for SemanticCheckWalker {
    fn visit_call(&mut self, node: &mut CallNode) -> Result<(), CompilerError> {
        for argument in &mut node.arguments {
            argument.visit(self)?;
        }

        // Check function existence.
        if !self.context.function_prototypes.contains_key(&node.name)
            && !EFUNS.contains_key(node.name.as_str())
        {
            let e = CompilerError::NewError(
                NewError::new(format!("Call to unknown function `{}`", node.name))
                    .with_span(node.span),
            );
            self.context.errors.push(e);
            // Non-fatal. Continue.
        }

        // Further checks require access to the function prototype for error messaging.
        let proto_opt = if let Some(prototype) = self.context.function_prototypes.get(&node.name) {
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
                let e = NewError::new(format!(
                    "Incorrect argument count in call to `{}`: expected: {}, received: {}",
                    node.name, prototype.num_args, arg_len
                ))
                .with_span(node.span)
                .with_label("Defined here", prototype.span);
                self.context.errors.push(CompilerError::NewError(e));
            }

            // Check argument types.
            for (index, ty) in prototype.arg_types.iter().enumerate() {
                if let Some(arg) = node.arguments.get(index) {
                    // Literal zero is always allowed
                    if let ExpressionNode::Int(IntNode { value: 0, .. }) = *arg {
                        // sigh.
                    } else {
                        let arg_type =
                            node_type(arg, &self.context.scopes, &self.function_return_values());
                        if !ty.matches_type(arg_type) {
                            let e = NewError::new(format!(
                                "Unexpected argument type to `{}`: {}. Expected {}.",
                                node.name, arg_type, ty
                            ))
                            .with_span(arg.span())
                            .with_label("Declared here", prototype.arg_spans.get(index).cloned());

                            self.context.errors.push(CompilerError::NewError(e));
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

        match check_binary_operation_types(
            node,
            &self.context.scopes,
            &self.function_return_values(),
        ) {
            Ok(_) => Ok(()),
            Err(err) => {
                let e = CompilerError::NewError(err);
                self.context.errors.push(e.clone());
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
                    let return_type = node_type(
                        expression,
                        &self.context.scopes,
                        &self.function_return_values(),
                    );

                    if function_def.return_type == LpcType::Void
                        || !function_def.return_type.matches_type(return_type)
                    {
                        let error = CompilerError::NewError(
                            NewError::new(format!(
                                "Invalid return type {}. Expected {}.",
                                return_type, function_def.return_type
                            ))
                            .with_span(node.span),
                        );

                        self.context.errors.push(error);
                    }
                }
            } else if function_def.return_type != LpcType::Void {
                let error = CompilerError::NewError(
                    NewError::new(format!(
                        "Invalid return type {}. Expected {}.",
                        LpcType::Void,
                        function_def.return_type
                    ))
                    .with_span(node.span),
                );

                self.context.errors.push(error);
            }
        } // else warn?

        Ok(())
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<(), CompilerError> {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;

            let expr_type = node_type(
                expression,
                &self.context.scopes,
                &self.function_return_values(),
            );

            let ret = if node.type_.matches_type(expr_type) {
                Ok(())
            } else if let ExpressionNode::Int(IntNode { value: 0, .. }) = *expression {
                // The integer 0 is always a valid assignment.
                Ok(())
            } else {
                let e = NewError::new(format!(
                    "Mismatched types: `{}` ({}) = `{}` ({})",
                    node.name, node.type_, expression, expr_type
                ))
                .with_span(node.span);

                let ce = CompilerError::NewError(e);
                self.context.errors.push(ce.clone());

                Err(ce)
            };

            return ret;
        }

        Ok(())
    }

    fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<(), CompilerError> {
        node.lhs.visit(self)?;
        node.rhs.visit(self)?;

        let left_type = node_type(
            &node.lhs,
            &self.context.scopes,
            &self.function_return_values(),
        );
        let right_type = node_type(
            &node.rhs,
            &self.context.scopes,
            &self.function_return_values(),
        );

        if left_type.matches_type(right_type) {
            Ok(())
        } else if let ExpressionNode::Int(IntNode { value: 0, .. }) = *node.rhs {
            // The integer 0 is always a valid assignment.
            Ok(())
        } else {
            let e = NewError::new(format!(
                "Mismatched types: `{}` ({}) = `{}` ({})",
                node.lhs, left_type, node.rhs, right_type
            ))
            .with_span(node.span);

            let ce = CompilerError::NewError(e);

            self.context.errors.push(ce.clone());

            Err(ce)
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
            node_type(left, &self.context.scopes, &self.function_return_values())
        } else {
            LpcType::Int(false)
        };

        let right_type = if let Some(right) = &*node.r {
            node_type(right, &self.context.scopes, &self.function_return_values())
        } else {
            LpcType::Int(false)
        };

        // These must resolve to ints at some point.
        let required_types = LpcType::Int(false) | LpcType::Mixed(false);

        if left_type.matches_type(required_types) && right_type.matches_type(required_types) {
            Ok(())
        } else {
            let left_val = if let Some(node) = &*node.l {
                format!("{}", node)
            } else {
                String::from("0")
            };

            let right_val = if let Some(node) = &*node.r {
                format!("{}", node)
            } else {
                String::from("-1")
            };

            let e = CompilerError::NewError(
                NewError::new(format!(
                    "Invalid range types: `{}` ({}) .. `{}` ({})",
                    left_val, left_type, right_val, right_type
                ))
                .with_span(node.span),
            );

            self.context.errors.push(e.clone());

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
        semantic::{
            function_prototype::FunctionPrototype, lpc_type::LpcType, scope_tree::ScopeTree,
            symbol::Symbol,
        },
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

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("known"),
                FunctionPrototype {
                    name: String::from("known"),
                    return_type: LpcType::Int(false),
                    num_args: 0,
                    num_default_args: 0,
                    arg_types: vec![],
                    span: None,
                    arg_spans: vec![],
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_call_allows_known_efuns() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
                name: "dump".to_string(),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_call_disallows_unknown_functions() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "unknown".to_string(),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.context.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_disallows_incorrect_function_arity() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "print".to_string(),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.context.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_understands_argument_defaults() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![],
                name: "my_func".to_string(),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: String::from("my_func"),
                    return_type: LpcType::Int(false),
                    num_args: 1,
                    num_default_args: 1,
                    arg_types: vec![LpcType::String(false)],
                    span: None,
                    arg_spans: vec![],
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_call_disallows_invalid_arg_types() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(123)],
                name: "my_func".to_string(),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: String::from("my_func"),
                    return_type: LpcType::Int(false),
                    num_args: 1,
                    num_default_args: 0,
                    arg_types: vec![LpcType::String(false)],
                    span: None,
                    arg_spans: vec![],
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.context.errors.len(), 1);
        }

        #[test]
        fn test_visit_call_allows_0() {
            let mut node = ExpressionNode::from(CallNode {
                arguments: vec![ExpressionNode::from(0)],
                name: "my_func".to_string(),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: String::from("my_func"),
                    return_type: LpcType::String(false),
                    num_args: 1,
                    num_default_args: 0,
                    arg_types: vec![LpcType::String(false)],
                    span: None,
                    arg_spans: vec![],
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Int(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Int(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            assert!(node.visit(&mut walker).is_err());
        }

        #[test]
        fn test_visit_assignment_allows_mixed() {
            let mut init_node = VarInitNode {
                type_: LpcType::Mixed(false),
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Mixed(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = init_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_assignment_allows_array_items() {
            let mut init_node = VarInitNode {
                type_: LpcType::Int(false),
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Mixed(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = init_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_assignment_allows_array_ranges() {
            let mut init_node = VarInitNode {
                type_: LpcType::Int(true),
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Mixed(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = init_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }
    }

    mod test_visit_var_init {
        use super::*;

        #[test]
        fn test_visit_var_init_validates_both_sides() {
            let mut node = VarInitNode {
                name: "foo".to_string(),
                type_: LpcType::Int(false),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
            };

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_var_init_always_allows_0() {
            let mut node = VarInitNode {
                type_: LpcType::String(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(0)),
                array: false,
                global: false,
                span: None,
            };

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_var_init_disallows_differing_types() {
            let mut node = VarInitNode {
                type_: LpcType::String(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
            };

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(!walker.context.errors.is_empty());
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
                return_type: LpcType::Void,
                name: "foo".to_string(),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let int_function_def = FunctionDefNode {
                return_type: LpcType::Int(false),
                name: "snuh".to_string(),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);

            // return void from void function
            walker.current_function = Some(void_function_def.clone());
            let _ = void_node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());

            // return void from non-void function
            walker.current_function = Some(int_function_def);
            let _ = void_node.visit(&mut walker);
            assert!(!walker.context.errors.is_empty());

            walker.context.errors = vec![];

            // return int from int function
            let _ = int_node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());

            // return int from void function
            walker.current_function = Some(void_function_def);
            let _ = int_node.visit(&mut walker);
            assert!(!walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_return_allows_0() {
            let mut node = ReturnNode {
                value: Some(ExpressionNode::from(0)),
                span: None,
            };

            let void_function_def = FunctionDefNode {
                return_type: LpcType::Void,
                name: "foo".to_string(),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            walker.current_function = Some(void_function_def);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_return_allows_mixed() {
            let mut node = ReturnNode {
                value: Some(ExpressionNode::from(123)),
                span: None,
            };

            let function_def = FunctionDefNode {
                return_type: LpcType::Mixed(false),
                name: "foo".to_string(),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            walker.current_function = Some(function_def);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
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

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Int(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_range_disallows_non_ints() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::Var(VarNode::new("foo")))),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            scopes.get_current_mut().unwrap().insert(sym);
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(!walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_range_allows_start_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(None),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_range_allows_end_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::from(456))),
                r: Box::new(None),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn test_visit_range_allows_both_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(None),
                r: Box::new(None),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = Context {
                scopes,
                function_prototypes,
                ..Context::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }
    }
}
