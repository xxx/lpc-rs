use crate::semantic::local_scope::LocalScope;
use crate::errors::{VarRedefinitionError, BinaryOperationError};
use crate::ast::var_init_node::VarInitNode;
use crate::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
use crate::semantic::scope_tree::ScopeTree;
use crate::semantic::lpc_type::LPCVarType;
use crate::ast::int_node::IntNode;
use crate::ast::string_node::StringNode;
use crate::ast::var_node::VarNode;
use crate::ast::expression_node::ExpressionNode;

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
                                  -> Result<(), VarRedefinitionError<'a>> {
    if let Some(sym) = scope.lookup(&node.name) {
        Err(VarRedefinitionError {
            symbol: &sym,
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
/// * `node` - The node we're checking to see if it's a redefinition
/// * `scope_tree` - A reference to the scope tree that holds the program symbols
pub fn check_binary_operation(node: &BinaryOpNode, scope_tree: &ScopeTree)
                              -> Result<(), BinaryOperationError> {
    fn create_error(
        node: &BinaryOpNode,
        op: BinaryOperation,
        left_type: LPCVarType,
        right_type: LPCVarType,
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
        node_type(&node.l, scope_tree),
        node_type(&node.r, scope_tree)
    );

    match node.op {
        BinaryOperation::Add => {
            match tuple {
                (LPCVarType::Int, LPCVarType::Int) => Ok(()),
                (LPCVarType::String, LPCVarType::Int) => Ok(()),
                (LPCVarType::String, LPCVarType::String) => Ok(()),
                (left_type, right_type) =>
                    Err(create_error(node, BinaryOperation::Add, left_type, right_type))
            }
        }
        BinaryOperation::Sub => {
            match tuple {
                (LPCVarType::Int, LPCVarType::Int) => Ok(()),
                (left_type, right_type) =>
                    Err(create_error(node, BinaryOperation::Sub, left_type, right_type))
            }
        }
        BinaryOperation::Mul => {
            match tuple {
                (LPCVarType::Int, LPCVarType::Int) => Ok(()),
                (LPCVarType::String, LPCVarType::Int) => Ok(()),
                (left_type, right_type) =>
                    Err(create_error(node, BinaryOperation::Mul, left_type, right_type))
            }
        }
        BinaryOperation::Div => {
            match tuple {
                (LPCVarType::Int, LPCVarType::Int) => Ok(()),
                (left_type, right_type) =>
                    Err(create_error(node, BinaryOperation::Div, left_type, right_type))
            }
        }
    }
}

fn node_type(node: &ExpressionNode, scope_tree: &ScopeTree) -> LPCVarType {
    match node {
        ExpressionNode::Int(IntNode { .. }) => LPCVarType::Int,
        ExpressionNode::String(StringNode { .. }) => LPCVarType::String,
        ExpressionNode::Var(VarNode { name }) => {
            match scope_tree.lookup(name) {
                Some(sym) => {
                    sym.type_
                }
                _ => panic!("undefined symbol {}", name)
            }
        }
        ExpressionNode::BinaryOp(BinaryOpNode { l, .. }) => {
            node_type(l, scope_tree)
        }
        &_ => unimplemented!()
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
                type_: LPCVarType::Int,
                ..Default::default()
            };
            let int2 = Symbol {
                name: "int2".to_string(),
                type_: LPCVarType::Int,
                ..Default::default()
            };
            let string1 = Symbol {
                name: "string1".to_string(),
                type_: LPCVarType::String,
                ..Default::default()
            };
            let string2 = Symbol {
                name: "string2".to_string(),
                type_: LPCVarType::String,
                ..Default::default()
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

            check_binary_operation(&node, &scope_tree)
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
                ExpressionNode::from(VarNode { name: "int1".to_string() }),
                ExpressionNode::from(VarNode { name: "int2".to_string() }),
                &scope_tree
            )
        }

        fn string_int_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(VarNode { name: "string2".to_string() }),
                ExpressionNode::from(VarNode { name: "int2".to_string() }),
                &scope_tree
            )
        }

        fn int_string_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(VarNode { name: "int2".to_string() }),
                ExpressionNode::from(VarNode { name: "string2".to_string() }),
                &scope_tree
            )
        }

        fn string_string_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
            get_result(
                op,
                ExpressionNode::from(VarNode { name: "string1".to_string() }),
                ExpressionNode::from(VarNode { name: "string2".to_string() }),
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
                            ExpressionNode::from(VarNode { name: "string1".to_string() })
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
                            ExpressionNode::from(VarNode { name: "int2".to_string() })
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
                            ExpressionNode::from(VarNode { name: "int1".to_string() })
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(VarNode { name: "string2".to_string() })),
                            r: Box::new(ExpressionNode::from(VarNode { name: "int2".to_string() })),
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
                            ExpressionNode::from(VarNode { name: "int1".to_string() })
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
                            ExpressionNode::from(VarNode { name: "int2".to_string() })
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
                            ExpressionNode::from(VarNode { name: "int1".to_string() })
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(VarNode { name: "string2".to_string() })),
                            r: Box::new(ExpressionNode::from(VarNode { name: "int2".to_string() })),
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
                            ExpressionNode::from(VarNode { name: "int1".to_string() })
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
                            ExpressionNode::from(VarNode { name: "int2".to_string() })
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
                            ExpressionNode::from(VarNode { name: "int1".to_string() })
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(VarNode { name: "string2".to_string() })),
                            r: Box::new(ExpressionNode::from(VarNode { name: "int2".to_string() })),
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
                            ExpressionNode::from(VarNode { name: "int1".to_string() })
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
                            ExpressionNode::from(VarNode { name: "int2".to_string() })
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
                            ExpressionNode::from(VarNode { name: "int1".to_string() })
                        ),
                        op: BinaryOperation::Add,
                        span: None
                    }),
                    ExpressionNode::from(BinaryOpNode {
                        l: Box::new(ExpressionNode::from(BinaryOpNode {
                            l: Box::new(ExpressionNode::from(VarNode { name: "string2".to_string() })),
                            r: Box::new(ExpressionNode::from(VarNode { name: "int2".to_string() })),
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