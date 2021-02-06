use std::collections::HashMap;
use crate::semantic::local_scope::LocalScope;
use crate::errors;
use errors::binary_operation_error::BinaryOperationError;
use errors::var_redefinition_error::VarRedefinitionError;
use crate::ast::var_init_node::VarInitNode;
use crate::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
use crate::semantic::scope_tree::ScopeTree;
use crate::semantic::lpc_type::LPCType;
use crate::ast::int_node::IntNode;
use crate::ast::string_node::StringNode;
use crate::ast::var_node::VarNode;
use crate::ast::expression_node::ExpressionNode;
use crate::ast::assignment_node::AssignmentNode;
use crate::ast::call_node::CallNode;
use crate::ast::array_node::ArrayNode;

/// Utility functions for doing various semantic checks.

/// Check if a var has already been defined in the local scope.
///
/// # Arguments
///
/// * `node` - The node we're checking to see if it's a redefinition
/// * `scope` - The scope to check
///
/// # Returns
///
/// A `Result` with either `Ok(())` or `Err(<error object>)`
pub fn check_var_redefinition<'a>(node: &'_ VarInitNode, scope: &'a LocalScope)
                                  -> Result<(), VarRedefinitionError> {
    if let Some(sym) = scope.lookup(&node.name) {
        Err(VarRedefinitionError {
            symbol: sym.clone(),
            span: node.span,
        })
    } else {
        Ok(())
    }
}

/// Check if a binary operation has mismatched types
///
/// # Arguments
///
/// * `node` - The node we're checking to see if it's being used incorrectly
/// * `scope_tree` - A reference to the scope tree that holds the program symbols
/// * `function_return_types` - A reference to a mapping of function names to their return types
pub fn check_binary_operation_types(
    node: &BinaryOpNode,
    scope_tree: &ScopeTree,
    function_return_types: &HashMap<&str, LPCType>) -> Result<(), BinaryOperationError> {
    fn create_error(
        node: &BinaryOpNode,
        op: BinaryOperation,
        left_type: LPCType,
        right_type: LPCType,
    ) -> BinaryOperationError {
        BinaryOperationError {
            op,
            left_name: format!("{}", node.l),
            left_type,
            right_name: format!("{}", node.r),
            right_type,
            span: node.span,
        }
    }

    let tuple = (
        node_type(&node.l, scope_tree, function_return_types),
        node_type(&node.r, scope_tree, function_return_types)
    );

    if let LPCType::Mixed(_) = tuple.0 {
        return if tuple.0.matches_type(tuple.1) {
            Ok(())
        } else {
            Err(create_error(node, node.op, tuple.0, tuple.1))
        }
    } else if let LPCType::Mixed(_) = tuple.1 {
        return if tuple.0.matches_type(tuple.1) {
            Ok(())
        } else {
            Err(create_error(node, node.op, tuple.0, tuple.1))
        }
    }

    match node.op {
        BinaryOperation::Add => {
            match tuple {
                (LPCType::Int(false), LPCType::Int(false)) => Ok(()),
                (LPCType::String(false), LPCType::Int(false)) => Ok(()),
                (LPCType::String(false), LPCType::String(false)) => Ok(()),
                (left_type, right_type) =>
                    Err(create_error(node, BinaryOperation::Add, left_type, right_type))
            }
        }
        BinaryOperation::Sub => {
            match tuple {
                (LPCType::Int(false), LPCType::Int(false)) => Ok(()),
                (left_type, right_type) =>
                    Err(create_error(node, BinaryOperation::Sub, left_type, right_type))
            }
        }
        BinaryOperation::Mul => {
            match tuple {
                (LPCType::Int(false), LPCType::Int(false)) => Ok(()),
                (LPCType::String(false), LPCType::Int(false)) => Ok(()),
                (left_type, right_type) =>
                    Err(create_error(node, BinaryOperation::Mul, left_type, right_type))
            }
        }
        BinaryOperation::Div => {
            match tuple {
                (LPCType::Int(false), LPCType::Int(false)) => Ok(()),
                (left_type, right_type) =>
                    Err(create_error(node, BinaryOperation::Div, left_type, right_type))
            }
        }
    }
}

/// Resolve an expression node down to a single type, recursively if necessary
pub fn node_type(
    node: &ExpressionNode,
    scope_tree: &ScopeTree,
    function_return_types: &HashMap<&str, LPCType>) -> LPCType {
    match node {
        ExpressionNode::Call(CallNode { name, .. }) => {
            if let Some(return_type) = function_return_types.get(name.as_str()) {
                *return_type
            } else {
                LPCType::Int(false)
            }
        },
        ExpressionNode::Int(IntNode { .. }) => LPCType::Int(false),
        ExpressionNode::String(StringNode { .. }) => LPCType::String(false),
        ExpressionNode::Var(VarNode { name, .. }) => {
            match scope_tree.lookup(name) {
                Some(sym) => {
                    sym.type_
                }
                _ => panic!("undefined symbol {}", name)
            }
        }
        ExpressionNode::BinaryOp(BinaryOpNode { l, .. }) => {
            node_type(l, scope_tree, function_return_types)
        }
        ExpressionNode::Assignment(AssignmentNode { lhs, .. }) => {
            node_type(lhs, scope_tree, function_return_types)
        }
        ExpressionNode::Array(ArrayNode { value, .. }) => {
            if value.len() == 0 {
                LPCType::Int(false)
            } else {
                // we arbitrarily take the type of the first item in the list.
                node_type(&value[0], scope_tree, function_return_types)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod check_binary_operation_tests {
        use super::*;
        use crate::semantic::symbol::Symbol;

        fn setup() -> ScopeTree {
            let int1 = Symbol {
                name: "int1".to_string(),
                type_: LPCType::Int(false),
                static_: false,
                location: None,
                scope_id: 0,
                span: None
            };
            let int2 = Symbol {
                name: "int2".to_string(),
                type_: LPCType::Int(false),
                static_: false,
                location: None,
                scope_id: 0,
                span: None
            };
            let string1 = Symbol {
                name: "string1".to_string(),
                type_: LPCType::String(false),
                static_: false,
                location: None,
                scope_id: 0,
                span: None
            };
            let string2 = Symbol {
                name: "string2".to_string(),
                type_: LPCType::String(false),
                static_: false,
                location: None,
                scope_id: 0,
                span: None
            };

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let scope = scope_tree.get_current_mut().unwrap();
            scope.insert(int1);
            scope.insert(int2);
            scope.insert(string1);
            scope.insert(string2);

            scope_tree
        }

        fn get_result(
            op: BinaryOperation,
            left_node: ExpressionNode,
            right_node: ExpressionNode,
            scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            let node = BinaryOpNode {
                l: Box::new(left_node),
                r: Box::new(right_node),
                op,
                span: None,
            };
            let function_return_types = HashMap::new();

            check_binary_operation_types(&node, &scope_tree, &function_return_types)
        }

        fn int_int_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(123),
                ExpressionNode::from(-123),
                &scope_tree
            )
        }

        fn string_int_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from("foo"),
                ExpressionNode::from(123),
                &scope_tree
            )
        }

        fn int_string_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(123),
                ExpressionNode::from("foo"),
                &scope_tree
            )
        }

        fn string_string_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from("asdf"),
                ExpressionNode::from("foo"),
                &scope_tree
            )
        }
        
        fn int_int_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("int1")),
                ExpressionNode::from(VarNode::new("int2")),
                &scope_tree
            )
        }

        fn string_int_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("string2")),
                ExpressionNode::from(VarNode::new("int2")),
                &scope_tree
            )
        }

        fn int_string_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("int2")),
                ExpressionNode::from(VarNode::new("string2")),
                &scope_tree
            )
        }

        fn string_string_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("string1")),
                ExpressionNode::from(VarNode::new("string2")),
                &scope_tree
            )
        }
        
        #[test]
        fn test_add() {
            let scope_tree = setup();

            assert!(int_int_literals(BinaryOperation::Add, &scope_tree).is_ok());
            assert!(string_string_literals(BinaryOperation::Add, &scope_tree).is_ok());
            assert!(string_int_literals(BinaryOperation::Add, &scope_tree).is_ok());
            assert!(int_string_literals(BinaryOperation::Add, &scope_tree).is_err());

            assert!(int_int_vars(BinaryOperation::Add, &scope_tree).is_ok());
            assert!(string_string_vars(BinaryOperation::Add, &scope_tree).is_ok());
            assert!(string_int_vars(BinaryOperation::Add, &scope_tree).is_ok());
            assert!(int_string_vars(BinaryOperation::Add, &scope_tree).is_err());

            // valid complex tree
            assert!(
                get_result(
                    BinaryOperation::Add,
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from("foo")),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("string1"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(-123)),
                            r: Box::new(ExpressionNode::from(82)),
                            op: BinaryOperation::Mul,
                            span: None
                        })),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int2"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    &scope_tree,
                ).is_ok()
            );

            // invalid complex tree
            assert!(
                get_result(
                    BinaryOperation::Add,
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(222)),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int1"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(VarNode::new("string2"))),
                            r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                            op: BinaryOperation::Mul,
                            span: None
                        })),
                        r: Box::new(
                            ExpressionNode::from(-123)
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    &scope_tree,
                ).is_err()
            );
        }
        
        #[test]
        fn test_sub() {
            let scope_tree = setup();

            assert!(int_int_literals(BinaryOperation::Sub, &scope_tree).is_ok());
            assert!(string_string_literals(BinaryOperation::Sub, &scope_tree).is_err());
            assert!(string_int_literals(BinaryOperation::Sub, &scope_tree).is_err());
            assert!(int_string_literals(BinaryOperation::Sub, &scope_tree).is_err());

            assert!(int_int_vars(BinaryOperation::Sub, &scope_tree).is_ok());
            assert!(string_string_vars(BinaryOperation::Sub, &scope_tree).is_err());
            assert!(string_int_vars(BinaryOperation::Sub, &scope_tree).is_err());
            assert!(int_string_vars(BinaryOperation::Sub, &scope_tree).is_err());

            // valid complex tree
            assert!(
                get_result(
                    BinaryOperation::Sub,
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(123)),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int1"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(-123)),
                            r: Box::new(ExpressionNode::from(82)),
                            op: BinaryOperation::Mul,
                            span: None
                        })),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int2"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    &scope_tree,
                ).is_ok()
            );

            // invalid complex tree
            assert!(
                get_result(
                    BinaryOperation::Sub,
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(222)),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int1"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(VarNode::new("string2"))),
                            r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                            op: BinaryOperation::Mul,
                            span: None
                        })),
                        r: Box::new(
                            ExpressionNode::from(-123)
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    &scope_tree,
                ).is_err()
            );
        }

        #[test]
        fn test_mul() {
            let scope_tree = setup();

            assert!(int_int_literals(BinaryOperation::Mul, &scope_tree).is_ok());
            assert!(string_string_literals(BinaryOperation::Mul, &scope_tree).is_err());
            assert!(string_int_literals(BinaryOperation::Mul, &scope_tree).is_ok());
            assert!(int_string_literals(BinaryOperation::Mul, &scope_tree).is_err());

            assert!(int_int_vars(BinaryOperation::Mul, &scope_tree).is_ok());
            assert!(string_string_vars(BinaryOperation::Mul, &scope_tree).is_err());
            assert!(string_int_vars(BinaryOperation::Mul, &scope_tree).is_ok());
            assert!(int_string_vars(BinaryOperation::Mul, &scope_tree).is_err());

            // valid complex tree
            assert!(
                get_result(
                    BinaryOperation::Mul,
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(123)),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int1"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(-123)),
                            r: Box::new(ExpressionNode::from(82)),
                            op: BinaryOperation::Mul,
                            span: None
                        })),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int2"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    &scope_tree,
                ).is_ok()
            );

            // invalid complex tree
            assert!(
                get_result(
                    BinaryOperation::Mul,
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(222)),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int1"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(VarNode::new("string2"))),
                            r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                            op: BinaryOperation::Mul,
                            span: None
                        })),
                        r: Box::new(
                            ExpressionNode::from(-123)
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    &scope_tree,
                ).is_err()
            );
        }
        
        #[test]
        fn test_div() {
            let scope_tree = setup();

            assert!(int_int_literals(BinaryOperation::Div, &scope_tree).is_ok());
            assert!(string_string_literals(BinaryOperation::Div, &scope_tree).is_err());
            assert!(string_int_literals(BinaryOperation::Div, &scope_tree).is_err());
            assert!(int_string_literals(BinaryOperation::Div, &scope_tree).is_err());

            assert!(int_int_vars(BinaryOperation::Div, &scope_tree).is_ok());
            assert!(string_string_vars(BinaryOperation::Div, &scope_tree).is_err());
            assert!(string_int_vars(BinaryOperation::Div, &scope_tree).is_err());
            assert!(int_string_vars(BinaryOperation::Div, &scope_tree).is_err());

            // valid complex tree
            assert!(
                get_result(
                    BinaryOperation::Div,
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(123)),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int1"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(-123)),
                            r: Box::new(ExpressionNode::from(82)),
                            op: BinaryOperation::Mul,
                            span: None
                        })),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int2"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    &scope_tree,
                ).is_ok()
            );

            // invalid complex tree
            assert!(
                get_result(
                    BinaryOperation::Div,
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(222)),
                        r: Box::new(
                            ExpressionNode::from(VarNode::new("int1"))
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(VarNode::new("string2"))),
                            r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                            op: BinaryOperation::Mul,
                            span: None
                        })),
                        r: Box::new(
                            ExpressionNode::from(-123)
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    &scope_tree,
                ).is_err()
            );
        }
    }
}