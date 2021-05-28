use phf::phf_set;
use std::collections::HashMap;

use crate::{
    ast::{
        assignment_node::AssignmentNode,
        ast_node::SpannedNode,
        binary_op_node::{BinaryOpNode, BinaryOperation},
        call_node::CallNode,
        comma_expression_node::CommaExpressionNode,
        expression_node::ExpressionNode,
        unary_op_node::{UnaryOpNode, UnaryOperation},
        var_init_node::VarInitNode,
        var_node::VarNode,
    },
    errors::LpcError,
    semantic::{local_scope::LocalScope, lpc_type::LpcType, scope_tree::ScopeTree},
    Result,
};

/// Utility functions for doing various semantic checks.

static KEYWORDS: phf::Set<&'static str> = phf_set! {
    "break",
    "case",
    "continue",
    "default", // TODO: add token
    "do",
    "efun",
    "else",
    "float",
    "for",
    "foreach", // TODO: add token
    "function", // TODO: add token
    "if",
    "inherit",
    "int",
    "mapping",
    "mixed",
    "nomask",
    "object",
    "private", // TODO: add token
    "public", // TODO: add token
    "return",
    "static",
    "string",
    "switch", // TODO: add token
    "void",
    "while",
};

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
pub fn check_var_redefinition(node: &'_ VarInitNode, scope: &'_ LocalScope) -> Result<()> {
    if let Some(sym) = scope.lookup(&node.name) {
        Err(LpcError::new(format!("Redefinition of `{}`", sym.name)).with_span(node.span))
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
    function_return_types: &HashMap<&str, LpcType>,
) -> Result<()> {
    fn create_error(
        node: &BinaryOpNode,
        op: BinaryOperation,
        left_type: LpcType,
        right_type: LpcType,
    ) -> LpcError {
        LpcError::new(format!(
            "Mismatched types: `{}` ({}) {} `{}` ({})",
            node.l, left_type, op, node.r, right_type
        ))
        .with_span(node.span)
    }

    let left_type = node_type(&node.l, scope_tree, function_return_types)?;
    let right_type = node_type(&node.r, scope_tree, function_return_types)?;
    let tuple = (left_type, right_type);

    if node.op != BinaryOperation::Index {
        let handle = |tuple: (LpcType, LpcType), node| {
            if tuple.0.matches_type(tuple.1) {
                Ok(())
            } else {
                Err(create_error(node, node.op, tuple.0, tuple.1))
            }
        };

        if matches!(tuple.0, LpcType::Mixed(_)) || matches!(tuple.1, LpcType::Mixed(_)) {
            return handle(tuple, node);
        }
    }

    match node.op {
        BinaryOperation::Add => {
            // Arrays have no type at runtime, so concatenation is always allowed.
            if tuple.0.is_array() && tuple.1.is_array() {
                return Ok(());
            }

            match tuple {
                (LpcType::Int(false), LpcType::Int(false))
                | (LpcType::String(false), LpcType::Int(false))
                | (LpcType::Int(false), LpcType::String(false))
                | (LpcType::String(false), LpcType::String(false))
                | (LpcType::Float(false), LpcType::Float(false))
                | (LpcType::Float(false), LpcType::Int(false))
                | (LpcType::Int(false), LpcType::Float(false))
                | (LpcType::Mapping(false), LpcType::Mapping(false)) => Ok(()),
                (left_type, right_type) => Err(create_error(
                    node,
                    BinaryOperation::Add,
                    left_type,
                    right_type,
                )),
            }
        }
        BinaryOperation::Sub => {
            if tuple.0.is_array() && tuple.1.is_array() {
                return Ok(());
            }

            match tuple {
                (LpcType::Int(false), LpcType::Int(false))
                | (LpcType::Float(false), LpcType::Float(false))
                | (LpcType::Int(false), LpcType::Float(false))
                | (LpcType::Float(false), LpcType::Int(false)) => Ok(()),
                (left_type, right_type) => Err(create_error(
                    node,
                    BinaryOperation::Sub,
                    left_type,
                    right_type,
                )),
            }
        }
        BinaryOperation::Mul => match tuple {
            (LpcType::Int(false), LpcType::Int(false))
            | (LpcType::String(false), LpcType::Int(false))
            | (LpcType::Int(false), LpcType::String(false))
            | (LpcType::Float(false), LpcType::Float(false))
            | (LpcType::Int(false), LpcType::Float(false))
            | (LpcType::Float(false), LpcType::Int(false)) => Ok(()),
            (left_type, right_type) => Err(create_error(
                node,
                BinaryOperation::Mul,
                left_type,
                right_type,
            )),
        },
        BinaryOperation::Div => match tuple {
            (LpcType::Int(false), LpcType::Int(false))
            | (LpcType::Float(false), LpcType::Float(false))
            | (LpcType::Int(false), LpcType::Float(false))
            | (LpcType::Float(false), LpcType::Int(false)) => Ok(()),
            (left_type, right_type) => Err(create_error(
                node,
                BinaryOperation::Div,
                left_type,
                right_type,
            )),
        },
        BinaryOperation::Index => {
            if matches!(left_type, LpcType::Mapping(_))
                || (left_type.is_array()
                    && (right_type == LpcType::Int(false)
                        || matches!(*node.r, ExpressionNode::Range(_))))
            {
                Ok(())
            } else {
                Err(create_error(
                    node,
                    BinaryOperation::Index,
                    left_type,
                    right_type,
                ))
            }
        }
        BinaryOperation::AndAnd => todo!(),
        BinaryOperation::OrOr => todo!(),
        BinaryOperation::EqEq => Ok(()),
    }
}

/// Check if a unary operation has mismatched types
///
/// # Arguments
///
/// * `node` - The node we're checking to see if it's being used incorrectly
/// * `scope_tree` - A reference to the scope tree that holds the program symbols
/// * `function_return_types` - A reference to a mapping of function names to their return types
pub fn check_unary_operation_types(
    node: &UnaryOpNode,
    scope_tree: &ScopeTree,
    function_return_types: &HashMap<&str, LpcType>,
) -> Result<()> {
    let expr_type = node_type(&node.expr, scope_tree, function_return_types)?;

    let create_error = |expected| {
        LpcError::new(format!(
            "Invalid Type: `{}` `{}` ({}). Expected {}",
            node.op, node.expr, expr_type, expected
        ))
        .with_span(node.span)
    };

    match node.op {
        UnaryOperation::Negate => match expr_type {
            LpcType::Int(false) | LpcType::Float(false) => Ok(()),

            _ => Err(create_error("`int`, or `float`")),
        },
        UnaryOperation::Inc => todo!(),
        UnaryOperation::Dec => todo!(),
        UnaryOperation::Bang => todo!(),
        UnaryOperation::Tilde => todo!(),
    }
}

/// Check two types, and return the promotion if one occurs (or the same type if both are the same)
/// Returns the first type if no promotion is possible.
fn combine_types(type1: LpcType, type2: LpcType, op: BinaryOperation) -> LpcType {
    if op == BinaryOperation::Index {
        if matches!(type1, LpcType::Mapping(_)) {
            return LpcType::Mixed(false);
        }

        return type1.as_array(type2.is_array());
    }

    if type1 == type2
        || (!type1.is_array() && type2.is_array())
        || (type1.is_array() && !type2.is_array())
    {
        return type1;
    }

    // array <op> array is always a mixed array
    if type1.is_array() && type2.is_array() {
        return LpcType::Mixed(true);
    }

    match (type1, type2) {
        (LpcType::Int(false), LpcType::String(false))
        | (LpcType::String(false), LpcType::Int(false)) => LpcType::String(false),
        (x, _) => x,
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
/// The `LpcType` of the passed node.
pub fn node_type(
    node: &ExpressionNode,
    scope_tree: &ScopeTree,
    function_return_types: &HashMap<&str, LpcType>,
) -> Result<LpcType> {
    match node {
        ExpressionNode::Assignment(AssignmentNode { lhs, .. }) => {
            node_type(lhs, scope_tree, function_return_types)
        }
        ExpressionNode::Call(CallNode { name, .. }) => function_return_types
            .get(name.as_str())
            .map_or(Ok(LpcType::Int(false)), |return_type| Ok(*return_type)),
        ExpressionNode::CommaExpression(CommaExpressionNode { value, .. }) => {
            if !value.is_empty() {
                let len = value.len();
                node_type(&value[len - 1], scope_tree, function_return_types)
            } else {
                Err(
                    LpcError::new("We've somehow created an empty CommaExpression node")
                        .with_span(node.span()),
                )
            }
        }
        ExpressionNode::Float(_) => Ok(LpcType::Float(false)),
        ExpressionNode::Int(_) => Ok(LpcType::Int(false)),
        ExpressionNode::Range(_) => Ok(LpcType::Int(false)),
        ExpressionNode::String(_) => Ok(LpcType::String(false)),
        ExpressionNode::Var(VarNode { name, span, .. }) => match scope_tree.lookup(name) {
            Some(sym) => Ok(sym.type_),
            None => {
                return Err(LpcError::new(format!("undefined symbol {}", name)).with_span(*span))
            }
        },
        ExpressionNode::BinaryOp(BinaryOpNode { l, r, op, .. }) => Ok(combine_types(
            node_type(l, scope_tree, function_return_types)?,
            node_type(r, scope_tree, function_return_types)?,
            *op,
        )),
        ExpressionNode::UnaryOp(UnaryOpNode { expr, .. }) => {
            Ok(node_type(expr, scope_tree, function_return_types)?)
        }
        ExpressionNode::Array(node) => {
            if node.value.is_empty() {
                return Ok(LpcType::Mixed(true));
            }

            let res: Result<Vec<_>> = node
                .value
                .iter()
                .map(|i| node_type(i, scope_tree, function_return_types))
                .collect();

            let value_types = match res {
                Ok(x) => x,
                Err(e) => return Err(e),
            };

            if value_types.iter().any(|ty| ty.is_array()) {
                Ok(LpcType::Mixed(true))
            } else if value_types.windows(2).all(|w| w[0] == w[1]) {
                Ok(value_types[0].as_array(true))
            } else {
                Ok(LpcType::Mixed(true))
            }
        }
        ExpressionNode::Mapping(_) => Ok(LpcType::Mapping(false)),
    }
}

/// Is the passed name a keyword?
pub fn is_keyword<T>(name: T) -> Result<()>
where
    T: AsRef<str>,
{
    if KEYWORDS.contains(name.as_ref()) {
        return Err(LpcError::new(format!(
            "`{}` is a keyword of the language, and cannot be used here.",
            name.as_ref()
        )));
    }

    Ok(())
}

#[cfg(test)]
mod check_binary_operation_tests {
    use super::*;
    use crate::{ast::range_node::RangeNode, semantic::symbol::Symbol};

    fn setup() -> ScopeTree {
        let int1 = Symbol {
            name: "int1".to_string(),
            type_: LpcType::Int(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let int2 = Symbol {
            name: "int2".to_string(),
            type_: LpcType::Int(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let string1 = Symbol {
            name: "string1".to_string(),
            type_: LpcType::String(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let string2 = Symbol {
            name: "string2".to_string(),
            type_: LpcType::String(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let array1 = Symbol {
            name: "array1".to_string(),
            type_: LpcType::Int(true),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let array2 = Symbol {
            name: "array2".to_string(),
            type_: LpcType::Int(true),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let float1 = Symbol {
            name: "float1".to_string(),
            type_: LpcType::Float(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let float2 = Symbol {
            name: "float2".to_string(),
            type_: LpcType::Float(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let mapping1 = Symbol {
            name: "mapping1".to_string(),
            type_: LpcType::Mapping(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let mapping2 = Symbol {
            name: "mapping2".to_string(),
            type_: LpcType::Mapping(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
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
        scope.insert(float1);
        scope.insert(float2);
        scope.insert(mapping1);
        scope.insert(mapping2);

        scope_tree
    }

    fn get_result(
        op: BinaryOperation,
        left_node: ExpressionNode,
        right_node: ExpressionNode,
        scope_tree: &ScopeTree,
    ) -> Result<()> {
        let node = BinaryOpNode {
            l: Box::new(left_node),
            r: Box::new(right_node),
            op,
            span: None,
        };
        let function_return_types = HashMap::new();

        check_binary_operation_types(&node, &scope_tree, &function_return_types)
    }

    fn int_int_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(123),
            ExpressionNode::from(-123),
            &scope_tree,
        )
    }

    fn string_int_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from("foo"),
            ExpressionNode::from(123),
            &scope_tree,
        )
    }

    fn int_string_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(123),
            ExpressionNode::from("foo"),
            &scope_tree,
        )
    }

    fn string_string_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from("asdf"),
            ExpressionNode::from("foo"),
            &scope_tree,
        )
    }

    fn int_int_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("int1")),
            ExpressionNode::from(VarNode::new("int2")),
            &scope_tree,
        )
    }

    fn string_int_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("string2")),
            ExpressionNode::from(VarNode::new("int2")),
            &scope_tree,
        )
    }

    fn int_string_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("int2")),
            ExpressionNode::from(VarNode::new("string2")),
            &scope_tree,
        )
    }

    fn string_string_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("string1")),
            ExpressionNode::from(VarNode::new("string2")),
            &scope_tree,
        )
    }

    fn array_array_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(vec!["asdf", "bar", "hi"]),
            ExpressionNode::from(vec![1, 2, 5]),
            &scope_tree,
        )
    }

    fn array_array_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("array1")),
            ExpressionNode::from(VarNode::new("array2")),
            &scope_tree,
        )
    }

    fn array_int_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(vec!["asdf", "bar", "hi"]),
            ExpressionNode::from(666),
            &scope_tree,
        )
    }

    fn array_int_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("array1")),
            ExpressionNode::from(VarNode::new("int1")),
            &scope_tree,
        )
    }

    fn array_range_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(vec![666, 2]),
            ExpressionNode::Range(RangeNode::new(Some(ExpressionNode::from(0)), None, None)),
            &scope_tree,
        )
    }

    fn array_range_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("array1")),
            ExpressionNode::Range(RangeNode::new(
                Some(ExpressionNode::from(VarNode::new("int1"))),
                Some(ExpressionNode::from(VarNode::new("int2"))),
                None,
            )),
            &scope_tree,
        )
    }

    fn float_float_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(123.45),
            ExpressionNode::from(-123.45),
            &scope_tree,
        )
    }

    fn float_float_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("float1")),
            ExpressionNode::from(VarNode::new("float2")),
            &scope_tree,
        )
    }

    fn float_int_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(123.45),
            ExpressionNode::from(-123),
            &scope_tree,
        )
    }

    fn float_int_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("float1")),
            ExpressionNode::from(VarNode::new("int1")),
            &scope_tree,
        )
    }

    fn int_float_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(-123),
            ExpressionNode::from(123.45),
            &scope_tree,
        )
    }

    fn int_float_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("int1")),
            ExpressionNode::from(VarNode::new("float1")),
            &scope_tree,
        )
    }

    fn mapping_mapping_literals(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(HashMap::new()),
            ExpressionNode::from(HashMap::new()),
            &scope_tree,
        )
    }

    fn mapping_mapping_vars(op: BinaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("mapping1")),
            ExpressionNode::from(VarNode::new("mapping2")),
            &scope_tree,
        )
    }

    #[test]
    fn test_add() {
        let scope_tree = setup();

        assert!(int_int_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(string_string_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(float_float_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(string_int_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(int_string_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(float_int_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(int_float_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(array_array_literals(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(array_int_literals(BinaryOperation::Add, &scope_tree).is_err());
        assert!(array_range_literals(BinaryOperation::Add, &scope_tree).is_err());
        assert!(mapping_mapping_literals(BinaryOperation::Add, &scope_tree).is_ok());

        assert!(int_int_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(string_string_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(float_float_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(string_int_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(int_string_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(float_int_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(int_float_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(array_array_vars(BinaryOperation::Add, &scope_tree).is_ok());
        assert!(array_int_vars(BinaryOperation::Add, &scope_tree).is_err());
        assert!(array_range_vars(BinaryOperation::Add, &scope_tree).is_err());
        assert!(mapping_mapping_vars(BinaryOperation::Add, &scope_tree).is_ok());

        // valid complex tree
        assert!(get_result(
            BinaryOperation::Add,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from("foo")),
                r: Box::new(ExpressionNode::from(VarNode::new("string1"))),
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
                r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_ok());

        // invalid complex tree
        assert!(get_result(
            BinaryOperation::Add,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![123])),
                r: Box::new(ExpressionNode::from(VarNode::new("int1"))),
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
                r: Box::new(ExpressionNode::from(-123)),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_err());
    }

    #[test]
    fn test_sub() {
        let scope_tree = setup();

        assert!(int_int_literals(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(string_string_literals(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(float_float_literals(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(string_int_literals(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(int_string_literals(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(float_int_literals(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(int_float_literals(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(array_array_literals(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(array_int_literals(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(array_range_literals(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(mapping_mapping_literals(BinaryOperation::Sub, &scope_tree).is_err());

        assert!(int_int_vars(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(string_string_vars(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(float_float_vars(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(string_int_vars(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(int_string_vars(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(float_int_vars(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(int_float_vars(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(array_array_vars(BinaryOperation::Sub, &scope_tree).is_ok());
        assert!(array_int_vars(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(array_range_vars(BinaryOperation::Sub, &scope_tree).is_err());
        assert!(mapping_mapping_vars(BinaryOperation::Sub, &scope_tree).is_err());

        // valid complex tree
        assert!(get_result(
            BinaryOperation::Sub,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from(VarNode::new("int1"))),
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
                r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_ok());

        // invalid complex tree
        assert!(get_result(
            BinaryOperation::Sub,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(222)),
                r: Box::new(ExpressionNode::from(VarNode::new("int1"))),
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
                r: Box::new(ExpressionNode::from(-123)),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_err());
    }

    #[test]
    fn test_mul() {
        let scope_tree = setup();

        assert!(int_int_literals(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(string_string_literals(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(float_float_literals(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(string_int_literals(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(int_string_literals(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(float_int_literals(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(int_float_literals(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(array_array_literals(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(array_int_literals(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(array_range_literals(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(mapping_mapping_literals(BinaryOperation::Mul, &scope_tree).is_err());

        assert!(int_int_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(string_string_vars(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(float_float_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(string_int_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(int_string_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(float_int_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(int_float_vars(BinaryOperation::Mul, &scope_tree).is_ok());
        assert!(array_array_vars(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(array_int_vars(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(array_range_vars(BinaryOperation::Mul, &scope_tree).is_err());
        assert!(mapping_mapping_vars(BinaryOperation::Mul, &scope_tree).is_err());

        // valid complex tree
        assert!(get_result(
            BinaryOperation::Mul,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from(VarNode::new("int1"))),
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
                r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_ok());

        // invalid complex tree
        assert!(get_result(
            BinaryOperation::Mul,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![222])),
                r: Box::new(ExpressionNode::from(VarNode::new("int1"))),
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
                r: Box::new(ExpressionNode::from(-123)),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_err());
    }

    #[test]
    fn test_div() {
        let scope_tree = setup();

        assert!(int_int_literals(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(string_string_literals(BinaryOperation::Div, &scope_tree).is_err());
        assert!(float_float_literals(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(string_int_literals(BinaryOperation::Div, &scope_tree).is_err());
        assert!(int_string_literals(BinaryOperation::Div, &scope_tree).is_err());
        assert!(float_int_literals(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(int_float_literals(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(array_array_literals(BinaryOperation::Div, &scope_tree).is_err());
        assert!(array_int_literals(BinaryOperation::Div, &scope_tree).is_err());
        assert!(array_range_literals(BinaryOperation::Div, &scope_tree).is_err());
        assert!(mapping_mapping_literals(BinaryOperation::Div, &scope_tree).is_err());

        assert!(int_int_vars(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(string_string_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(float_float_vars(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(string_int_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(int_string_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(float_int_vars(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(int_float_vars(BinaryOperation::Div, &scope_tree).is_ok());
        assert!(array_array_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(array_int_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(array_range_vars(BinaryOperation::Div, &scope_tree).is_err());
        assert!(mapping_mapping_vars(BinaryOperation::Div, &scope_tree).is_err());

        // valid complex tree
        assert!(get_result(
            BinaryOperation::Div,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from(VarNode::new("int1"))),
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
                r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_ok());

        // invalid complex tree
        assert!(get_result(
            BinaryOperation::Div,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(222)),
                r: Box::new(ExpressionNode::from(VarNode::new("int1"))),
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
                r: Box::new(ExpressionNode::from(-123)),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_err());
    }

    #[test]
    fn test_index() {
        let scope_tree = setup();

        assert!(int_int_literals(BinaryOperation::Index, &scope_tree).is_err());
        assert!(string_string_literals(BinaryOperation::Index, &scope_tree).is_err());
        assert!(string_int_literals(BinaryOperation::Index, &scope_tree).is_err());
        assert!(int_string_literals(BinaryOperation::Index, &scope_tree).is_err());
        assert!(array_array_literals(BinaryOperation::Index, &scope_tree).is_err());
        assert!(array_int_literals(BinaryOperation::Index, &scope_tree).is_ok());
        assert!(array_range_literals(BinaryOperation::Index, &scope_tree).is_ok());
        assert!(mapping_mapping_literals(BinaryOperation::Index, &scope_tree).is_ok());

        assert!(int_int_vars(BinaryOperation::Index, &scope_tree).is_err());
        assert!(string_string_vars(BinaryOperation::Index, &scope_tree).is_err());
        assert!(string_int_vars(BinaryOperation::Index, &scope_tree).is_err());
        assert!(int_string_vars(BinaryOperation::Index, &scope_tree).is_err());
        assert!(array_array_vars(BinaryOperation::Index, &scope_tree).is_err());
        assert!(array_int_vars(BinaryOperation::Index, &scope_tree).is_ok());
        assert!(array_range_vars(BinaryOperation::Index, &scope_tree).is_ok());
        assert!(mapping_mapping_vars(BinaryOperation::Index, &scope_tree).is_ok());

        // valid complex tree
        assert!(get_result(
            BinaryOperation::Index,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec!["foo"])),
                r: Box::new(ExpressionNode::from(VarNode::new("array1"))),
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
                r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_ok());

        // invalid complex tree
        assert!(get_result(
            BinaryOperation::Index,
            ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![123])),
                r: Box::new(ExpressionNode::from(VarNode::new("int1"))),
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
                r: Box::new(ExpressionNode::from(-123)),
                op: BinaryOperation::Add,
                span: None
            }),
            &scope_tree,
        )
        .is_err());
    }
}

#[cfg(test)]
mod check_unary_operation_tests {
    use super::*;
    use crate::semantic::symbol::Symbol;

    fn setup() -> ScopeTree {
        let int1 = Symbol {
            name: "int1".to_string(),
            type_: LpcType::Int(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let string1 = Symbol {
            name: "string1".to_string(),
            type_: LpcType::String(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let array1 = Symbol {
            name: "array1".to_string(),
            type_: LpcType::Int(true),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let float1 = Symbol {
            name: "float1".to_string(),
            type_: LpcType::Float(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };
        let mapping1 = Symbol {
            name: "mapping1".to_string(),
            type_: LpcType::Mapping(false),
            static_: false,
            location: None,
            scope_id: 0,
            span: None,
        };

        let mut scope_tree = ScopeTree::default();
        scope_tree.push_new();
        let scope = scope_tree.get_current_mut().unwrap();
        scope.insert(int1);
        scope.insert(string1);
        scope.insert(array1);
        scope.insert(float1);
        scope.insert(mapping1);

        scope_tree
    }

    fn get_result(
        op: UnaryOperation,
        expr_node: ExpressionNode,
        scope_tree: &ScopeTree,
    ) -> Result<()> {
        let node = UnaryOpNode {
            expr: Box::new(expr_node),
            op,
            is_post: false,
            span: None,
        };
        let function_return_types = HashMap::new();

        check_unary_operation_types(&node, &scope_tree, &function_return_types)
    }

    fn int_literal(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(op, ExpressionNode::from(123), &scope_tree)
    }

    fn string_literal(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(op, ExpressionNode::from("foo"), &scope_tree)
    }

    fn int_var(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(op, ExpressionNode::from(VarNode::new("int1")), &scope_tree)
    }

    fn string_var(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("string2")),
            &scope_tree,
        )
    }

    fn array_literal(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(vec!["asdf", "bar", "hi"]),
            &scope_tree,
        )
    }

    fn array_var(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("array1")),
            &scope_tree,
        )
    }

    fn float_literal(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(op, ExpressionNode::from(123.45), &scope_tree)
    }

    fn float_var(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("float1")),
            &scope_tree,
        )
    }

    fn mapping_literal(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(op, ExpressionNode::from(HashMap::new()), &scope_tree)
    }

    fn mapping_var(op: UnaryOperation, scope_tree: &ScopeTree) -> Result<()> {
        get_result(
            op,
            ExpressionNode::from(VarNode::new("mapping1")),
            &scope_tree,
        )
    }

    #[test]
    fn test_add() {
        let scope_tree = setup();

        assert!(int_literal(UnaryOperation::Negate, &scope_tree).is_ok());
        assert!(int_var(UnaryOperation::Negate, &scope_tree).is_ok());
        assert!(float_literal(UnaryOperation::Negate, &scope_tree).is_ok());
        assert!(float_var(UnaryOperation::Negate, &scope_tree).is_ok());
        assert!(string_literal(UnaryOperation::Negate, &scope_tree).is_err());
        assert!(string_var(UnaryOperation::Negate, &scope_tree).is_err());
        assert!(array_literal(UnaryOperation::Negate, &scope_tree).is_err());
        assert!(array_var(UnaryOperation::Negate, &scope_tree).is_err());
        assert!(mapping_literal(UnaryOperation::Negate, &scope_tree).is_err());
        assert!(mapping_var(UnaryOperation::Negate, &scope_tree).is_err());
    }
}

#[cfg(test)]
mod combine_types_tests {
    use super::*;

    #[test]
    fn index_into_mapping_is_mixed() {
        let combo = combine_types(
            LpcType::Mapping(false),
            LpcType::String(false),
            BinaryOperation::Index,
        );
        assert_eq!(combo, LpcType::Mixed(false));
    }

    #[test]
    fn index_into_array_is_first_type_with_second_type_array_status() {
        let combo = combine_types(
            LpcType::String(true),
            LpcType::Int(false),
            BinaryOperation::Index,
        );
        assert_eq!(combo, LpcType::String(false));
    }

    #[test]
    fn equivalent_returns_that_type() {
        let type_ = LpcType::String(true);
        let combo = combine_types(type_, type_, BinaryOperation::Add);
        assert_eq!(combo, type_);
    }

    #[test]
    fn differing_array_status_returns_first_type() {
        let combo = combine_types(
            LpcType::String(true),
            LpcType::Int(false),
            BinaryOperation::Add,
        );
        assert_eq!(combo, LpcType::String(true));

        let combo = combine_types(
            LpcType::String(false),
            LpcType::Int(true),
            BinaryOperation::Add,
        );
        assert_eq!(combo, LpcType::String(false));
    }

    #[test]
    fn both_array_is_mixed_array() {
        let combo = combine_types(
            LpcType::String(true),
            LpcType::Int(true),
            BinaryOperation::Add,
        );
        assert_eq!(combo, LpcType::Mixed(true));
    }

    #[test]
    fn both_int_op_string_is_string() {
        let combo = combine_types(
            LpcType::String(false),
            LpcType::Int(false),
            BinaryOperation::Add,
        );
        assert_eq!(combo, LpcType::String(false));

        let combo = combine_types(
            LpcType::Int(false),
            LpcType::String(false),
            BinaryOperation::Add,
        );
        assert_eq!(combo, LpcType::String(false));
    }
}

#[cfg(test)]
mod node_type_tests {
    use super::*;

    mod arrays {
        use super::*;
        use crate::ast::array_node::ArrayNode;

        #[test]
        fn test_node_type_empty_array_is_mixed() {
            let node = ExpressionNode::Array(ArrayNode {
                value: vec![],
                span: None,
            });
            let scope_tree = ScopeTree::default();
            let function_return_types = HashMap::new();

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types).unwrap(),
                LpcType::Mixed(true)
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
                node_type(&node, &scope_tree, &function_return_types).unwrap(),
                LpcType::Int(true)
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
                node_type(&node, &scope_tree, &function_return_types).unwrap(),
                LpcType::Mixed(true)
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
                span: None,
            });

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types).unwrap(),
                LpcType::String(false)
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
                span: None,
            });

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types).unwrap(),
                LpcType::String(false)
            );
        }

        #[test]
        fn test_node_type_comma_expression_is_last_item() {
            let scope_tree = ScopeTree::default();
            let function_return_types = HashMap::new();

            let node = ExpressionNode::CommaExpression(CommaExpressionNode {
                value: vec![ExpressionNode::from(123), ExpressionNode::from("foobar")],
                span: None,
            });

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types).unwrap(),
                LpcType::String(false)
            );
        }
    }

    mod binary_ops {
        use super::*;
        use crate::semantic::symbol::Symbol;

        #[test]
        fn test_index_array_returns_singular_of_left_type() {
            let mut scope_tree = ScopeTree::default();
            let id = scope_tree.push_new();
            let scope = scope_tree.get_mut(id).unwrap();
            scope.insert(Symbol {
                name: "foo".to_string(),
                type_: LpcType::Int(true),
                static_: false,
                location: None,
                scope_id: 0,
                span: None,
            });
            let function_return_types = HashMap::new();

            let l = ExpressionNode::Var(VarNode {
                name: "foo".to_string(),
                span: None,
                global: true,
            });
            let r = ExpressionNode::from(1);

            let node = ExpressionNode::BinaryOp(BinaryOpNode {
                l: Box::new(l),
                r: Box::new(r),
                op: BinaryOperation::Index,
                span: None,
            });

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types).unwrap(),
                LpcType::Int(false)
            );
        }

        #[test]
        fn test_index_mapping_is_mixed() {
            let mut scope_tree = ScopeTree::default();
            let id = scope_tree.push_new();
            let scope = scope_tree.get_mut(id).unwrap();
            scope.insert(Symbol {
                name: "foo".to_string(),
                type_: LpcType::Mapping(false),
                static_: false,
                location: None,
                scope_id: 0,
                span: None,
            });
            let function_return_types = HashMap::new();

            let l = ExpressionNode::Var(VarNode {
                name: "foo".to_string(),
                span: None,
                global: true,
            });
            let r = ExpressionNode::from(1);

            let node = ExpressionNode::BinaryOp(BinaryOpNode {
                l: Box::new(l),
                r: Box::new(r),
                op: BinaryOperation::Index,
                span: None,
            });

            assert_eq!(
                node_type(&node, &scope_tree, &function_return_types).unwrap(),
                LpcType::Mixed(false)
            );
        }
    }
}
