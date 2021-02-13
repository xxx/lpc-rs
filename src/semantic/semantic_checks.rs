use std::collections::HashMap;
use crate::semantic::local_scope::LocalScope;
use crate::errors;
use errors::compiler_error::binary_operation_error::BinaryOperationError;
use errors::compiler_error::var_redefinition_error::VarRedefinitionError;
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
            // Arrays have no type at runtime, so concatenation is always allowed.
            if tuple.0.is_array() && tuple.1.is_array() {
                return Ok(());
            }

            match tuple {
                (LPCType::Int(false), LPCType::Int(false)) => Ok(()),
                (LPCType::String(false), LPCType::Int(false)) => Ok(()),
                (LPCType::Int(false), LPCType::String(false)) => Ok(()),
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
                (LPCType::Int(false), LPCType::String(false)) => Ok(()),
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

/// Check two types, and return the promotion if one occurs (or the same type if both are the same)
/// Returns the first type if no promotion is possible.
fn combine_types(type1: LPCType, type2: LPCType) -> LPCType {
    if type1 == type2 ||
        (!type1.is_array() && type2.is_array()) ||
        (type1.is_array() && !type2.is_array()) {

        return type1;
    }

    // array + array is always a mixed array
    if type1.is_array() && type2.is_array() {
        return LPCType::Mixed(true);
    }

    match (type1, type2) {
        (LPCType::Int(false), LPCType::String(false)) => LPCType::String(false),
        (LPCType::String(false), LPCType::Int(false)) => LPCType::String(false),
        (x, _) => x
    }
}

/// Resolve an expression node down to a single type, recursively if necessary
/// Handles type promotion when necessary.
///
/// # Arguments
/// `node` - The `ExpressionNode` whose type we would like to resolve.
/// `scope_tree` - A `ScopeTree`, with its current scope set to the start scope
///                    for resolving variables.
/// `function_return_types` - A `HashMap` of function names, to the type they return
///
/// # Returns
/// The `LPCType` of the passed node.
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
        ExpressionNode::BinaryOp(BinaryOpNode { l, r, .. }) => {
            combine_types(
                node_type(l, scope_tree, function_return_types),
                node_type(r, scope_tree, function_return_types)
            )
        }
        ExpressionNode::Assignment(AssignmentNode { lhs, .. }) => {
            node_type(lhs, scope_tree, function_return_types)
        }
        ExpressionNode::Array(node) => {
            if node.value.is_empty() {
                return LPCType::Mixed(true);
            }

            let value_types = node.value
                .iter()
                .map(|i| node_type(i, scope_tree, function_return_types))
                .collect::<Vec<_>>();

            if value_types.iter().any(|ty|
                  match *ty {
                      LPCType::Int(arr) => arr,
                      LPCType::String(arr) => arr,
                      LPCType::Float(arr) => arr,
                      LPCType::Object(arr) => arr,
                      LPCType::Mapping(arr) => arr,
                      LPCType::Mixed(arr) => arr,
                      _ => unimplemented!() // ExpressionNodes have a concrete type.
                  })
            {
                LPCType::Mixed(true)
            } else if value_types.windows(2).all(|w| w[0] == w[1]) {
                value_types[0].as_array(true)
            } else {
                LPCType::Mixed(true)
            }
        }
    }
}

#[cfg(test)]
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
        let array1 = Symbol {
            name: "array1".to_string(),
            type_: LPCType::Int(true),
            static_: false,
            location: None,
            scope_id: 0,
            span: None
        };
        let array2 = Symbol {
            name: "array2".to_string(),
            type_: LPCType::Int(true),
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
        scope.insert(array1);
        scope.insert(array2);

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

    fn array_array_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
        get_result(
            op,
            ExpressionNode::from(vec!["asdf", "bar", "hi"]),
            ExpressionNode::from(vec![1, 2, 5]),
            &scope_tree
        )
    }

    fn array_array_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<(), BinaryOperationError> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("array1")),
            ExpressionNode::from(VarNode::new("array2")),
            &scope_tree
        )
    }

    #[test]
    fn test_add() {
        let scope_tree = setup();

        assert!(int_int_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(string_string_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(string_int_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(int_string_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(array_array_literals(BinaryOperation::Add, &scope_tree).is_ok());

        assert!(int_int_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(string_string_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(string_int_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(int_string_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(array_array_vars(BinaryOperation::Add, &scope_tree).is_ok());

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
                    l: Box::new(ExpressionNode::from(vec![123])),
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
        assert!(array_array_literals(BinaryOperation::Sub, &scope_tree).is_err());

        assert!(int_int_vars(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(string_string_vars(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(string_int_vars(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(int_string_vars(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(array_array_vars(BinaryOperation::Sub, &scope_tree).is_err());

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
        assert!(int_string_literals(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(array_array_literals(BinaryOperation::Mul, &scope_tree).is_err());

        assert!(int_int_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(string_string_vars(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(string_int_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(int_string_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(array_array_vars(BinaryOperation::Mul, &scope_tree).is_err());

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
                    l: Box::new(ExpressionNode::from(vec![222])),
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
        assert!(array_array_literals(BinaryOperation::Div, &scope_tree).is_err());

        assert!(int_int_vars(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(string_string_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(string_int_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(int_string_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(array_array_vars(BinaryOperation::Div, &scope_tree).is_err());

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

#[cfg(test)]
mod check_node_type_tests {
    use super::*;

    mod arrays {
        use super::*;
        use crate::ast::array_node::ArrayNode;

        #[test]
        fn test_node_type_empty_array_is_mixed() {
            let node = ExpressionNode::Array(ArrayNode { value: vec![], span: None });
            let scope_tree = ScopeTree::default();
            let function_return_types = HashMap::new();

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types),
                LPCType::Mixed(true)
            );
        }

        #[test]
        fn test_node_type_array_all_same_is_that() {
            let scope_tree = ScopeTree::default();
            let function_return_types = HashMap::new();

            let node = ExpressionNode::from(vec![
                ExpressionNode::from(123),
                ExpressionNode::from(31),
                ExpressionNode::from(-4567),
                ExpressionNode::from(8238),
            ]);

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types),
                LPCType::Int(true)
            );
        }

        #[test]
        fn test_node_type_array_any_array_is_mixed() {
            let scope_tree = ScopeTree::default();
            let function_return_types = HashMap::new();

            let node = ExpressionNode::from(vec![
                ExpressionNode::from(vec![ExpressionNode::from(1)]),
                ExpressionNode::from(vec![ExpressionNode::from(34)]),
                ExpressionNode::from(vec![ExpressionNode::from(-123)]),
            ]);

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types),
                LPCType::Mixed(true)
            );
        }

        #[test]
        fn test_node_type_int_op_string_is_string() {
            let scope_tree = ScopeTree::default();
            let function_return_types = HashMap::new();

            let node = ExpressionNode::BinaryOp(BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from("asdf")),
                op: BinaryOperation::Add,
                span: None
            });

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types),
                LPCType::String(false)
            );
        }

        #[test]
        fn test_node_type_string_op_int_is_string() {
            let scope_tree = ScopeTree::default();
            let function_return_types = HashMap::new();

            let node = ExpressionNode::BinaryOp(BinaryOpNode {
                l: Box::new(ExpressionNode::from("asdf")),
                r: Box::new(ExpressionNode::from(23423)),
                op: BinaryOperation::Div,
                span: None
            });

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types),
                LPCType::String(false)
            );
        }
    }
}
