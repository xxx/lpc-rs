use lpc_rs_core::{call_namespace::CallNamespace, lpc_type::LpcType};
use lpc_rs_errors::{LpcError, Result};
use phf::phf_set;

use crate::compiler::{
    ast::{
        assignment_node::AssignmentNode,
        ast_node::SpannedNode,
        binary_op_node::{BinaryOpNode, BinaryOperation},
        call_node::CallNode,
        closure_node::ClosureNode,
        comma_expression_node::CommaExpressionNode,
        expression_node::ExpressionNode,
        ternary_node::TernaryNode,
        unary_op_node::{UnaryOpNode, UnaryOperation},
        var_init_node::VarInitNode,
        var_node::VarNode,
    },
    compilation_context::CompilationContext,
    semantic::local_scope::LocalScope,
};

/// Utility functions for doing various semantic checks.

static KEYWORDS: phf::Set<&'static str> = phf_set! {
    "break",
    "case",
    "catch",
    "continue",
    "default",
    "do",
    "efun",
    "else",
    "float",
    "for",
    "foreach",
    "function",
    "if",
    "inherit",
    "int",
    "mapping",
    "mixed",
    "nomask",
    "object",
    "private",
    "protected",
    "public",
    "return",
    "static",
    "string",
    "switch",
    "throw",
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
        Err(LpcError::new(format!("Redefinition of `{}`", sym.name))
            .with_span(node.span)
            .with_label("Originally declared here", sym.span))
    } else {
        Ok(())
    }
}

/// Check if a binary operation has mismatched types
///
/// # Arguments
///
/// * `node` - The node we're checking to see if it's being used incorrectly
/// * `context` - The current [`CompilationContext`]
pub fn check_binary_operation_types(
    node: &BinaryOpNode,
    context: &CompilationContext,
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

    let left_type = node_type(&node.l, context)?;
    let right_type = node_type(&node.r, context)?;
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
        BinaryOperation::Div | BinaryOperation::Mod => match tuple {
            (LpcType::Int(false), LpcType::Int(false))
            | (LpcType::Float(false), LpcType::Float(false))
            | (LpcType::Int(false), LpcType::Float(false))
            | (LpcType::Float(false), LpcType::Int(false)) => Ok(()),
            (left_type, right_type) => Err(create_error(node, node.op, left_type, right_type)),
        },
        BinaryOperation::Index => {
            if matches!(left_type, LpcType::Mapping(_) | LpcType::Mixed(_))
                || ((left_type.is_array() || matches!(left_type, LpcType::String(false)))
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
        BinaryOperation::AndAnd => Ok(()),
        BinaryOperation::OrOr => Ok(()),
        BinaryOperation::And
        | BinaryOperation::Or
        | BinaryOperation::Xor
        | BinaryOperation::Shl
        | BinaryOperation::Shr => match tuple {
            (LpcType::Int(false), LpcType::Int(false)) => Ok(()),
            (left_type, right_type) => Err(create_error(node, node.op, left_type, right_type)),
        },
        BinaryOperation::EqEq => Ok(()),
        BinaryOperation::Lt | BinaryOperation::Lte | BinaryOperation::Gt | BinaryOperation::Gte => {
            match tuple {
                (LpcType::Int(false), LpcType::Int(false))
                | (LpcType::Float(false), LpcType::Float(false))
                | (LpcType::String(false), LpcType::String(false)) => Ok(()),
                (left_type, right_type) => Err(create_error(node, node.op, left_type, right_type)),
            }
        }
    }
}

/// Check if a unary operation has mismatched types
///
/// # Arguments
///
/// * `node` - The node we're checking to see if it's being used incorrectly
/// * `context` - The current [`CompilationContext`]
pub fn check_unary_operation_types(node: &UnaryOpNode, context: &CompilationContext) -> Result<()> {
    let expr_type = node_type(&node.expr, context)?;

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
        UnaryOperation::Bang => Ok(()),
        UnaryOperation::Inc | UnaryOperation::Dec | UnaryOperation::BitwiseNot => match expr_type {
            LpcType::Int(false) => Ok(()),
            _ => Err(create_error("`int`")),
        },
    }
}

/// Check two types, and return the promotion if one occurs (or the same type if
/// both are the same) Returns the first type if no promotion is possible.
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
/// * `node` - The `ExpressionNode` whose type we would like to resolve.
/// * `context` - The current [`CompilationContext`]
///
/// # Returns
/// The `LpcType` of the passed node.
pub fn node_type(node: &ExpressionNode, context: &CompilationContext) -> Result<LpcType> {
    match node {
        ExpressionNode::Assignment(AssignmentNode { lhs, .. }) => node_type(lhs, context),
        ExpressionNode::Call(CallNode {
            name, namespace, ..
        }) => {
            // first check to see if we're calling a function pointer that's
            // overridden the function with this name
            context.lookup_var(name.as_str()).map_or_else(
                || {
                    context
                        .lookup_function_complete(name.as_str(), namespace)
                        // TODO: This `or` clause is where call_other checks end up
                        .map_or(Ok(LpcType::Mixed(false)), |function_like| {
                            Ok(function_like.as_ref().return_type)
                        })
                },
                |var| {
                    if var.type_.matches_type(LpcType::Function(false)) {
                        // TODO: Get the real return type here somehow
                        Ok(LpcType::Mixed(false))
                    } else {
                        Err(LpcError::new(format!(
                            "invalid call: `{}` is not a function",
                            name
                        )))
                    }
                },
            )
        }
        ExpressionNode::Closure(ClosureNode { return_type, .. }) => Ok(*return_type),
        ExpressionNode::CommaExpression(CommaExpressionNode { value, .. }) => {
            if !value.is_empty() {
                let len = value.len();
                node_type(&value[len - 1], context)
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
        ExpressionNode::Var(VarNode { name, span, .. }) => {
            if name.starts_with('$') {
                Ok(LpcType::Mixed(false))
            } else {
                match context.lookup_var(name) {
                    Some(sym) => Ok(sym.type_),
                    None => {
                        if context
                            .contains_function_complete(name.as_str(), &CallNamespace::default())
                        {
                            Ok(LpcType::Function(false))
                        } else {
                            return Err(LpcError::new(format!("undefined symbol {}", name))
                                .with_span(*span));
                        }
                    }
                }
            }
        }
        ExpressionNode::BinaryOp(BinaryOpNode { l, r, op, .. }) => Ok(combine_types(
            node_type(l, context)?,
            node_type(r, context)?,
            *op,
        )),
        ExpressionNode::UnaryOp(UnaryOpNode { expr, .. }) => Ok(node_type(expr, context)?),
        ExpressionNode::Array(node) => {
            if node.value.is_empty() {
                return Ok(LpcType::Mixed(true));
            }

            let res: Result<Vec<_>> = node.value.iter().map(|i| node_type(i, context)).collect();

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
        ExpressionNode::Ternary(TernaryNode { body, .. }) => Ok(node_type(body, context)?),
        ExpressionNode::Mapping(_) => Ok(LpcType::Mapping(false)),
        ExpressionNode::FunctionPtr(_) => Ok(LpcType::Function(false)),
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
mod tests {
    use factori::create;
    use lpc_rs_core::call_namespace::CallNamespace;
    use lpc_rs_function_support::symbol::Symbol;

    use super::*;
    use crate::test_support::factories::*;

    mod check_binary_operation_tests {
        use indexmap::IndexMap;
        use lpc_rs_function_support::symbol::Symbol;

        use super::*;
        use crate::compiler::{
            ast::range_node::RangeNode,
            semantic::{scope_tree::ScopeTree},
        };

        fn setup() -> CompilationContext {
            let int1 = Symbol {
                name: "int1".to_string(),
                type_: LpcType::Int(false),
                ..Default::default()
            };
            let int2 = Symbol {
                name: "int2".to_string(),
                type_: LpcType::Int(false),
                ..Default::default()
            };
            let string1 = Symbol {
                name: "string1".to_string(),
                type_: LpcType::String(false),
                ..Default::default()
            };
            let string2 = Symbol {
                name: "string2".to_string(),
                type_: LpcType::String(false),
                ..Default::default()
            };
            let array1 = Symbol {
                name: "array1".to_string(),
                type_: LpcType::Int(true),
                ..Default::default()
            };
            let array2 = Symbol {
                name: "array2".to_string(),
                type_: LpcType::Int(true),
                ..Default::default()
            };
            let float1 = Symbol {
                name: "float1".to_string(),
                type_: LpcType::Float(false),
                ..Default::default()
            };
            let float2 = Symbol {
                name: "float2".to_string(),
                type_: LpcType::Float(false),
                ..Default::default()
            };
            let mapping1 = Symbol {
                name: "mapping1".to_string(),
                type_: LpcType::Mapping(false),
                ..Default::default()
            };
            let mapping2 = Symbol {
                name: "mapping2".to_string(),
                type_: LpcType::Mapping(false),
                ..Default::default()
            };
            let mixed1 = Symbol {
                name: "mixed1".to_string(),
                type_: LpcType::Mixed(false),
                ..Default::default()
            };

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let scope = scope_tree.current_mut().unwrap();
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
            scope.insert(mixed1);

            CompilationContext {
                scopes: scope_tree,
                ..Default::default()
            }
        }

        fn get_result(
            op: BinaryOperation,
            left_node: ExpressionNode,
            right_node: ExpressionNode,
            context: &CompilationContext,
        ) -> Result<()> {
            let node = BinaryOpNode {
                l: Box::new(left_node),
                r: Box::new(right_node),
                op,
                span: None,
            };

            check_binary_operation_types(&node, &context)
        }

        fn int_int_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(123),
                ExpressionNode::from(-123),
                context,
            )
        }

        fn string_int_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from("foo"),
                ExpressionNode::from(123),
                context,
            )
        }

        fn int_string_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(123),
                ExpressionNode::from("foo"),
                context,
            )
        }

        fn string_string_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from("asdf"),
                ExpressionNode::from("foo"),
                context,
            )
        }

        fn int_int_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("int1")),
                ExpressionNode::from(VarNode::new("int2")),
                context,
            )
        }

        fn string_int_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("string2")),
                ExpressionNode::from(VarNode::new("int2")),
                context,
            )
        }

        fn int_string_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("int2")),
                ExpressionNode::from(VarNode::new("string2")),
                context,
            )
        }

        fn string_string_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("string1")),
                ExpressionNode::from(VarNode::new("string2")),
                context,
            )
        }

        fn array_array_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(vec!["asdf", "bar", "hi"]),
                ExpressionNode::from(vec![1, 2, 5]),
                context,
            )
        }

        fn array_array_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("array1")),
                ExpressionNode::from(VarNode::new("array2")),
                context,
            )
        }

        fn array_int_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(vec!["asdf", "bar", "hi"]),
                ExpressionNode::from(666),
                context,
            )
        }

        fn array_int_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("array1")),
                ExpressionNode::from(VarNode::new("int1")),
                context,
            )
        }

        fn array_range_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(vec![666, 2]),
                ExpressionNode::Range(RangeNode::new(Some(ExpressionNode::from(0)), None, None)),
                context,
            )
        }

        fn string_range_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from("foobarbazquux"),
                ExpressionNode::Range(RangeNode::new(Some(ExpressionNode::from(0)), None, None)),
                context,
            )
        }

        fn array_range_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("array1")),
                ExpressionNode::Range(RangeNode::new(
                    Some(ExpressionNode::from(VarNode::new("int1"))),
                    Some(ExpressionNode::from(VarNode::new("int2"))),
                    None,
                )),
                context,
            )
        }

        fn string_range_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("string1")),
                ExpressionNode::Range(RangeNode::new(
                    Some(ExpressionNode::from(VarNode::new("int1"))),
                    Some(ExpressionNode::from(VarNode::new("int2"))),
                    None,
                )),
                context,
            )
        }

        fn float_float_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(123.45),
                ExpressionNode::from(-123.45),
                context,
            )
        }

        fn float_float_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("float1")),
                ExpressionNode::from(VarNode::new("float2")),
                context,
            )
        }

        fn float_int_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(123.45),
                ExpressionNode::from(-123),
                context,
            )
        }

        fn float_int_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("float1")),
                ExpressionNode::from(VarNode::new("int1")),
                context,
            )
        }

        fn int_float_literals(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(-123),
                ExpressionNode::from(123.45),
                context,
            )
        }

        fn int_float_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("int1")),
                ExpressionNode::from(VarNode::new("float1")),
                context,
            )
        }

        fn mapping_mapping_literals(
            op: BinaryOperation,
            context: &CompilationContext,
        ) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(IndexMap::new()),
                ExpressionNode::from(IndexMap::new()),
                context,
            )
        }

        fn mapping_mapping_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("mapping1")),
                ExpressionNode::from(VarNode::new("mapping2")),
                context,
            )
        }

        fn mixed_any_vars(op: BinaryOperation, context: &CompilationContext) -> Result<()> {
            get_result(
                op,
                ExpressionNode::from(VarNode::new("mixed1")),
                ExpressionNode::from(VarNode::new("float1")),
                context,
            )
        }

        #[test]
        fn test_add() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Add, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::Add, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::Add, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::Add, &context).is_ok());
            assert!(int_string_literals(BinaryOperation::Add, &context).is_ok());
            assert!(float_int_literals(BinaryOperation::Add, &context).is_ok());
            assert!(int_float_literals(BinaryOperation::Add, &context).is_ok());
            assert!(array_array_literals(BinaryOperation::Add, &context).is_ok());
            assert!(array_int_literals(BinaryOperation::Add, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Add, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Add, &context).is_ok());

            assert!(int_int_vars(BinaryOperation::Add, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::Add, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::Add, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::Add, &context).is_ok());
            assert!(int_string_vars(BinaryOperation::Add, &context).is_ok());
            assert!(float_int_vars(BinaryOperation::Add, &context).is_ok());
            assert!(int_float_vars(BinaryOperation::Add, &context).is_ok());
            assert!(array_array_vars(BinaryOperation::Add, &context).is_ok());
            assert!(array_int_vars(BinaryOperation::Add, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Add, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Add, &context).is_ok());

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
                &context,
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
                &context,
            )
            .is_err());
        }

        #[test]
        fn test_sub() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Sub, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::Sub, &context).is_err());
            assert!(float_float_literals(BinaryOperation::Sub, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::Sub, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Sub, &context).is_err());
            assert!(float_int_literals(BinaryOperation::Sub, &context).is_ok());
            assert!(int_float_literals(BinaryOperation::Sub, &context).is_ok());
            assert!(array_array_literals(BinaryOperation::Sub, &context).is_ok());
            assert!(array_int_literals(BinaryOperation::Sub, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Sub, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Sub, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Sub, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::Sub, &context).is_err());
            assert!(float_float_vars(BinaryOperation::Sub, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::Sub, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Sub, &context).is_err());
            assert!(float_int_vars(BinaryOperation::Sub, &context).is_ok());
            assert!(int_float_vars(BinaryOperation::Sub, &context).is_ok());
            assert!(array_array_vars(BinaryOperation::Sub, &context).is_ok());
            assert!(array_int_vars(BinaryOperation::Sub, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Sub, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Sub, &context).is_err());

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
                &context,
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
                &context,
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
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Div, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::Div, &context).is_err());
            assert!(float_float_literals(BinaryOperation::Div, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::Div, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Div, &context).is_err());
            assert!(float_int_literals(BinaryOperation::Div, &context).is_ok());
            assert!(int_float_literals(BinaryOperation::Div, &context).is_ok());
            assert!(array_array_literals(BinaryOperation::Div, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Div, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Div, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Div, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Div, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::Div, &context).is_err());
            assert!(float_float_vars(BinaryOperation::Div, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::Div, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Div, &context).is_err());
            assert!(float_int_vars(BinaryOperation::Div, &context).is_ok());
            assert!(int_float_vars(BinaryOperation::Div, &context).is_ok());
            assert!(array_array_vars(BinaryOperation::Div, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Div, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Div, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Div, &context).is_err());

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
                &context,
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
                &context,
            )
            .is_err());
        }

        #[test]
        fn test_mod() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Mod, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::Mod, &context).is_err());
            assert!(float_float_literals(BinaryOperation::Mod, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::Mod, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Mod, &context).is_err());
            assert!(float_int_literals(BinaryOperation::Mod, &context).is_ok());
            assert!(int_float_literals(BinaryOperation::Mod, &context).is_ok());
            assert!(array_array_literals(BinaryOperation::Mod, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Mod, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Mod, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Mod, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Mod, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::Mod, &context).is_err());
            assert!(float_float_vars(BinaryOperation::Mod, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::Mod, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Mod, &context).is_err());
            assert!(float_int_vars(BinaryOperation::Mod, &context).is_ok());
            assert!(int_float_vars(BinaryOperation::Mod, &context).is_ok());
            assert!(array_array_vars(BinaryOperation::Mod, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Mod, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Mod, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Mod, &context).is_err());

            // valid complex tree
            assert!(get_result(
                BinaryOperation::Mod,
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
                        op: BinaryOperation::Mod,
                        span: None
                    })),
                    r: Box::new(ExpressionNode::from(VarNode::new("int2"))),
                    op: BinaryOperation::Add,
                    span: None
                }),
                &context,
            )
            .is_ok());

            // invalid complex tree
            assert!(get_result(
                BinaryOperation::Mod,
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
                        op: BinaryOperation::Mod,
                        span: None
                    })),
                    r: Box::new(ExpressionNode::from(-123)),
                    op: BinaryOperation::Add,
                    span: None
                }),
                &context,
            )
            .is_err());
        }

        #[test]
        fn test_index() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Index, &context).is_err());
            assert!(string_string_literals(BinaryOperation::Index, &context).is_err());
            assert!(string_int_literals(BinaryOperation::Index, &context).is_ok());
            assert!(int_string_literals(BinaryOperation::Index, &context).is_err());
            assert!(float_float_literals(BinaryOperation::Index, &context).is_err());
            assert!(array_array_literals(BinaryOperation::Index, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Index, &context).is_ok());
            assert!(array_range_literals(BinaryOperation::Index, &context).is_ok());
            assert!(string_range_literals(BinaryOperation::Index, &context).is_ok());
            assert!(mapping_mapping_literals(BinaryOperation::Index, &context).is_ok());

            assert!(int_int_vars(BinaryOperation::Index, &context).is_err());
            assert!(string_string_vars(BinaryOperation::Index, &context).is_err());
            assert!(string_int_vars(BinaryOperation::Index, &context).is_ok());
            assert!(int_string_vars(BinaryOperation::Index, &context).is_err());
            assert!(float_float_vars(BinaryOperation::Index, &context).is_err());
            assert!(array_array_vars(BinaryOperation::Index, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Index, &context).is_ok());
            assert!(array_range_vars(BinaryOperation::Index, &context).is_ok());
            assert!(string_range_vars(BinaryOperation::Index, &context).is_ok());
            assert!(mapping_mapping_vars(BinaryOperation::Index, &context).is_ok());
            assert!(mixed_any_vars(BinaryOperation::Index, &context).is_ok());

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
                &context,
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
                &context,
            )
            .is_err());
        }

        #[test]
        fn test_lt() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Lt, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::Lt, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::Lt, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::Lt, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Lt, &context).is_err());
            assert!(array_array_literals(BinaryOperation::Lt, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Lt, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Lt, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Lt, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Lt, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::Lt, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::Lt, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::Lt, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Lt, &context).is_err());
            assert!(array_array_vars(BinaryOperation::Lt, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Lt, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Lt, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Lt, &context).is_err());
        }

        #[test]
        fn test_lte() {
            let scope_tree = setup();

            assert!(int_int_literals(BinaryOperation::Lte, &scope_tree).is_ok());
            assert!(float_float_literals(BinaryOperation::Lte, &scope_tree).is_ok());
            assert!(string_string_literals(BinaryOperation::Lte, &scope_tree).is_ok());
            assert!(string_int_literals(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(int_string_literals(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(array_array_literals(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(array_int_literals(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(array_range_literals(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Lte, &scope_tree).is_err());

            assert!(int_int_vars(BinaryOperation::Lte, &scope_tree).is_ok());
            assert!(float_float_vars(BinaryOperation::Lte, &scope_tree).is_ok());
            assert!(string_string_vars(BinaryOperation::Lte, &scope_tree).is_ok());
            assert!(string_int_vars(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(int_string_vars(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(array_array_vars(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(array_int_vars(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(array_range_vars(BinaryOperation::Lte, &scope_tree).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Lte, &scope_tree).is_err());
        }

        #[test]
        fn test_gt() {
            let scope_tree = setup();

            assert!(int_int_literals(BinaryOperation::Gt, &scope_tree).is_ok());
            assert!(float_float_literals(BinaryOperation::Gt, &scope_tree).is_ok());
            assert!(string_string_literals(BinaryOperation::Gt, &scope_tree).is_ok());
            assert!(string_int_literals(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(int_string_literals(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(array_array_literals(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(array_int_literals(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(array_range_literals(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Gt, &scope_tree).is_err());

            assert!(int_int_vars(BinaryOperation::Gt, &scope_tree).is_ok());
            assert!(float_float_vars(BinaryOperation::Gt, &scope_tree).is_ok());
            assert!(string_string_vars(BinaryOperation::Gt, &scope_tree).is_ok());
            assert!(string_int_vars(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(int_string_vars(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(array_array_vars(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(array_int_vars(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(array_range_vars(BinaryOperation::Gt, &scope_tree).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Gt, &scope_tree).is_err());
        }

        #[test]
        fn test_gte() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Gte, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::Gte, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::Gte, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::Gte, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Gte, &context).is_err());
            assert!(array_array_literals(BinaryOperation::Gte, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Gte, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Gte, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Gte, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Gte, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::Gte, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::Gte, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::Gte, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Gte, &context).is_err());
            assert!(array_array_vars(BinaryOperation::Gte, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Gte, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Gte, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Gte, &context).is_err());
        }

        #[test]
        fn test_eqeq() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::EqEq, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::EqEq, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::EqEq, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::EqEq, &context).is_ok());
            assert!(int_string_literals(BinaryOperation::EqEq, &context).is_ok());
            assert!(array_array_literals(BinaryOperation::EqEq, &context).is_ok());
            assert!(array_int_literals(BinaryOperation::EqEq, &context).is_ok());
            assert!(array_range_literals(BinaryOperation::EqEq, &context).is_ok());
            assert!(mapping_mapping_literals(BinaryOperation::EqEq, &context).is_ok());

            assert!(int_int_vars(BinaryOperation::EqEq, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::EqEq, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::EqEq, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::EqEq, &context).is_ok());
            assert!(int_string_vars(BinaryOperation::EqEq, &context).is_ok());
            assert!(array_array_vars(BinaryOperation::EqEq, &context).is_ok());
            assert!(array_int_vars(BinaryOperation::EqEq, &context).is_ok());
            assert!(array_range_vars(BinaryOperation::EqEq, &context).is_ok());
            assert!(mapping_mapping_vars(BinaryOperation::EqEq, &context).is_ok());
        }

        #[test]
        fn test_andand() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::AndAnd, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::AndAnd, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::AndAnd, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::AndAnd, &context).is_ok());
            assert!(int_string_literals(BinaryOperation::AndAnd, &context).is_ok());
            assert!(array_array_literals(BinaryOperation::AndAnd, &context).is_ok());
            assert!(array_int_literals(BinaryOperation::AndAnd, &context).is_ok());
            assert!(array_range_literals(BinaryOperation::AndAnd, &context).is_ok());
            assert!(mapping_mapping_literals(BinaryOperation::AndAnd, &context).is_ok());

            assert!(int_int_vars(BinaryOperation::AndAnd, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::AndAnd, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::AndAnd, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::AndAnd, &context).is_ok());
            assert!(int_string_vars(BinaryOperation::AndAnd, &context).is_ok());
            assert!(array_array_vars(BinaryOperation::AndAnd, &context).is_ok());
            assert!(array_int_vars(BinaryOperation::AndAnd, &context).is_ok());
            assert!(array_range_vars(BinaryOperation::AndAnd, &context).is_ok());
            assert!(mapping_mapping_vars(BinaryOperation::AndAnd, &context).is_ok());
        }

        #[test]
        fn test_oror() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::OrOr, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::OrOr, &context).is_ok());
            assert!(string_string_literals(BinaryOperation::OrOr, &context).is_ok());
            assert!(string_int_literals(BinaryOperation::OrOr, &context).is_ok());
            assert!(int_string_literals(BinaryOperation::OrOr, &context).is_ok());
            assert!(array_array_literals(BinaryOperation::OrOr, &context).is_ok());
            assert!(array_int_literals(BinaryOperation::OrOr, &context).is_ok());
            assert!(array_range_literals(BinaryOperation::OrOr, &context).is_ok());
            assert!(mapping_mapping_literals(BinaryOperation::OrOr, &context).is_ok());

            assert!(int_int_vars(BinaryOperation::OrOr, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::OrOr, &context).is_ok());
            assert!(string_string_vars(BinaryOperation::OrOr, &context).is_ok());
            assert!(string_int_vars(BinaryOperation::OrOr, &context).is_ok());
            assert!(int_string_vars(BinaryOperation::OrOr, &context).is_ok());
            assert!(array_array_vars(BinaryOperation::OrOr, &context).is_ok());
            assert!(array_int_vars(BinaryOperation::OrOr, &context).is_ok());
            assert!(array_range_vars(BinaryOperation::OrOr, &context).is_ok());
            assert!(mapping_mapping_vars(BinaryOperation::OrOr, &context).is_ok());
        }

        #[test]
        fn test_and() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::And, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::And, &context).is_err());
            assert!(string_string_literals(BinaryOperation::And, &context).is_err());
            assert!(string_int_literals(BinaryOperation::And, &context).is_err());
            assert!(int_string_literals(BinaryOperation::And, &context).is_err());
            assert!(array_array_literals(BinaryOperation::And, &context).is_err());
            assert!(array_int_literals(BinaryOperation::And, &context).is_err());
            assert!(array_range_literals(BinaryOperation::And, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::And, &context).is_err());

            assert!(int_int_vars(BinaryOperation::And, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::And, &context).is_err());
            assert!(string_string_vars(BinaryOperation::And, &context).is_err());
            assert!(string_int_vars(BinaryOperation::And, &context).is_err());
            assert!(int_string_vars(BinaryOperation::And, &context).is_err());
            assert!(array_array_vars(BinaryOperation::And, &context).is_err());
            assert!(array_int_vars(BinaryOperation::And, &context).is_err());
            assert!(array_range_vars(BinaryOperation::And, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::And, &context).is_err());
        }

        #[test]
        fn test_or() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Or, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::Or, &context).is_err());
            assert!(string_string_literals(BinaryOperation::Or, &context).is_err());
            assert!(string_int_literals(BinaryOperation::Or, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Or, &context).is_err());
            assert!(array_array_literals(BinaryOperation::Or, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Or, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Or, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Or, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Or, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::Or, &context).is_err());
            assert!(string_string_vars(BinaryOperation::Or, &context).is_err());
            assert!(string_int_vars(BinaryOperation::Or, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Or, &context).is_err());
            assert!(array_array_vars(BinaryOperation::Or, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Or, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Or, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Or, &context).is_err());
        }

        #[test]
        fn test_xor() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Xor, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::Xor, &context).is_err());
            assert!(string_string_literals(BinaryOperation::Xor, &context).is_err());
            assert!(string_int_literals(BinaryOperation::Xor, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Xor, &context).is_err());
            assert!(array_array_literals(BinaryOperation::Xor, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Xor, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Xor, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Xor, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Xor, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::Xor, &context).is_err());
            assert!(string_string_vars(BinaryOperation::Xor, &context).is_err());
            assert!(string_int_vars(BinaryOperation::Xor, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Xor, &context).is_err());
            assert!(array_array_vars(BinaryOperation::Xor, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Xor, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Xor, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Xor, &context).is_err());
        }

        #[test]
        fn test_shl() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Shl, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::Shl, &context).is_err());
            assert!(string_string_literals(BinaryOperation::Shl, &context).is_err());
            assert!(string_int_literals(BinaryOperation::Shl, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Shl, &context).is_err());
            assert!(array_array_literals(BinaryOperation::Shl, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Shl, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Shl, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Shl, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Shl, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::Shl, &context).is_err());
            assert!(string_string_vars(BinaryOperation::Shl, &context).is_err());
            assert!(string_int_vars(BinaryOperation::Shl, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Shl, &context).is_err());
            assert!(array_array_vars(BinaryOperation::Shl, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Shl, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Shl, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Shl, &context).is_err());
        }

        #[test]
        fn test_shr() {
            let context = setup();

            assert!(int_int_literals(BinaryOperation::Shr, &context).is_ok());
            assert!(float_float_literals(BinaryOperation::Shr, &context).is_err());
            assert!(string_string_literals(BinaryOperation::Shr, &context).is_err());
            assert!(string_int_literals(BinaryOperation::Shr, &context).is_err());
            assert!(int_string_literals(BinaryOperation::Shr, &context).is_err());
            assert!(array_array_literals(BinaryOperation::Shr, &context).is_err());
            assert!(array_int_literals(BinaryOperation::Shr, &context).is_err());
            assert!(array_range_literals(BinaryOperation::Shr, &context).is_err());
            assert!(mapping_mapping_literals(BinaryOperation::Shr, &context).is_err());

            assert!(int_int_vars(BinaryOperation::Shr, &context).is_ok());
            assert!(float_float_vars(BinaryOperation::Shr, &context).is_err());
            assert!(string_string_vars(BinaryOperation::Shr, &context).is_err());
            assert!(string_int_vars(BinaryOperation::Shr, &context).is_err());
            assert!(int_string_vars(BinaryOperation::Shr, &context).is_err());
            assert!(array_array_vars(BinaryOperation::Shr, &context).is_err());
            assert!(array_int_vars(BinaryOperation::Shr, &context).is_err());
            assert!(array_range_vars(BinaryOperation::Shr, &context).is_err());
            assert!(mapping_mapping_vars(BinaryOperation::Shr, &context).is_err());
        }
    }

    mod check_unary_operation_tests {
        use indexmap::IndexMap;

        use super::*;
        use crate::compiler::semantic::{scope_tree::ScopeTree};

        fn setup() -> CompilationContext {
            let int1 = Symbol {
                name: "int1".to_string(),
                type_: LpcType::Int(false),
                ..Default::default()
            };
            let string1 = Symbol {
                name: "string1".to_string(),
                type_: LpcType::String(false),
                ..Default::default()
            };
            let array1 = Symbol {
                name: "array1".to_string(),
                type_: LpcType::Int(true),
                ..Default::default()
            };
            let float1 = Symbol {
                name: "float1".to_string(),
                type_: LpcType::Float(false),
                ..Default::default()
            };
            let mapping1 = Symbol {
                name: "mapping1".to_string(),
                type_: LpcType::Mapping(false),
                ..Default::default()
            };

            let mut scope_tree = ScopeTree::default();
            scope_tree.push_new();
            let scope = scope_tree.current_mut().unwrap();
            scope.insert(int1);
            scope.insert(string1);
            scope.insert(array1);
            scope.insert(float1);
            scope.insert(mapping1);

            CompilationContext {
                scopes: scope_tree,
                ..Default::default()
            }
        }

        fn to_result(
            op: UnaryOperation,
            expr_node: ExpressionNode,
            context: &CompilationContext,
        ) -> Result<()> {
            let node = UnaryOpNode {
                expr: Box::new(expr_node),
                op,
                is_post: false,
                span: None,
            };
            check_unary_operation_types(&node, &context)
        }

        fn int_literal(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(123), context)
        }

        fn string_literal(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from("foo"), context)
        }

        fn int_var(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(VarNode::new("int1")), context)
        }

        fn string_var(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(VarNode::new("string2")), context)
        }

        fn array_literal(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(vec!["asdf", "bar", "hi"]), context)
        }

        fn array_var(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(VarNode::new("array1")), context)
        }

        fn float_literal(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(123.45), context)
        }

        fn float_var(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(VarNode::new("float1")), context)
        }

        fn mapping_literal(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(IndexMap::new()), context)
        }

        fn mapping_var(op: UnaryOperation, context: &CompilationContext) -> Result<()> {
            to_result(op, ExpressionNode::from(VarNode::new("mapping1")), context)
        }

        #[test]
        fn test_negate() {
            let context = setup();

            assert!(int_literal(UnaryOperation::Negate, &context).is_ok());
            assert!(int_var(UnaryOperation::Negate, &context).is_ok());
            assert!(float_literal(UnaryOperation::Negate, &context).is_ok());
            assert!(float_var(UnaryOperation::Negate, &context).is_ok());
            assert!(string_literal(UnaryOperation::Negate, &context).is_err());
            assert!(string_var(UnaryOperation::Negate, &context).is_err());
            assert!(array_literal(UnaryOperation::Negate, &context).is_err());
            assert!(array_var(UnaryOperation::Negate, &context).is_err());
            assert!(mapping_literal(UnaryOperation::Negate, &context).is_err());
            assert!(mapping_var(UnaryOperation::Negate, &context).is_err());
        }

        #[test]
        fn test_inc() {
            let context = setup();

            assert!(int_literal(UnaryOperation::Inc, &context).is_ok());
            assert!(int_var(UnaryOperation::Inc, &context).is_ok());
            assert!(float_literal(UnaryOperation::Inc, &context).is_err());
            assert!(float_var(UnaryOperation::Inc, &context).is_err());
            assert!(string_literal(UnaryOperation::Inc, &context).is_err());
            assert!(string_var(UnaryOperation::Inc, &context).is_err());
            assert!(array_literal(UnaryOperation::Inc, &context).is_err());
            assert!(array_var(UnaryOperation::Inc, &context).is_err());
            assert!(mapping_literal(UnaryOperation::Inc, &context).is_err());
            assert!(mapping_var(UnaryOperation::Inc, &context).is_err());
        }

        #[test]
        fn test_dec() {
            let context = setup();

            assert!(int_literal(UnaryOperation::Dec, &context).is_ok());
            assert!(int_var(UnaryOperation::Dec, &context).is_ok());
            assert!(float_literal(UnaryOperation::Dec, &context).is_err());
            assert!(float_var(UnaryOperation::Dec, &context).is_err());
            assert!(string_literal(UnaryOperation::Dec, &context).is_err());
            assert!(string_var(UnaryOperation::Dec, &context).is_err());
            assert!(array_literal(UnaryOperation::Dec, &context).is_err());
            assert!(array_var(UnaryOperation::Dec, &context).is_err());
            assert!(mapping_literal(UnaryOperation::Dec, &context).is_err());
            assert!(mapping_var(UnaryOperation::Dec, &context).is_err());
        }

        #[test]
        fn test_bitwise_not() {
            let context = setup();

            assert!(int_literal(UnaryOperation::BitwiseNot, &context).is_ok());
            assert!(int_var(UnaryOperation::BitwiseNot, &context).is_ok());
            assert!(float_literal(UnaryOperation::BitwiseNot, &context).is_err());
            assert!(float_var(UnaryOperation::BitwiseNot, &context).is_err());
            assert!(string_literal(UnaryOperation::BitwiseNot, &context).is_err());
            assert!(string_var(UnaryOperation::BitwiseNot, &context).is_err());
            assert!(array_literal(UnaryOperation::BitwiseNot, &context).is_err());
            assert!(array_var(UnaryOperation::BitwiseNot, &context).is_err());
            assert!(mapping_literal(UnaryOperation::BitwiseNot, &context).is_err());
            assert!(mapping_var(UnaryOperation::BitwiseNot, &context).is_err());
        }
    }

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

    mod test_node_type {
        use super::*;

        mod arrays {
            use lpc_rs_core::{function_arity::FunctionArity, function_flags::FunctionFlags, EFUN};
            use lpc_rs_function_support::function_prototype::FunctionPrototype;

            use super::*;
            use crate::compiler::ast::array_node::ArrayNode;

            #[test]
            fn empty_array_is_mixed() {
                let node = ExpressionNode::Array(ArrayNode {
                    value: vec![],
                    span: None,
                });
                let context = CompilationContext::default();

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Mixed(true));
            }

            #[test]
            fn array_all_same_is_that() {
                let context = CompilationContext::default();

                let node = ExpressionNode::from(vec![
                    ExpressionNode::from(123),
                    ExpressionNode::from(31),
                    ExpressionNode::from(-4567),
                    ExpressionNode::from(8238),
                ]);

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Int(true));
            }

            #[test]
            fn array_any_array_is_mixed() {
                let context = CompilationContext::default();

                let node = ExpressionNode::from(vec![
                    ExpressionNode::from(vec![ExpressionNode::from(1)]),
                    ExpressionNode::from(vec![ExpressionNode::from(34)]),
                    ExpressionNode::from(vec![ExpressionNode::from(-123)]),
                ]);

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Mixed(true));
            }

            #[test]
            fn int_op_string_is_string() {
                let context = CompilationContext::default();

                let node = ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(123)),
                    r: Box::new(ExpressionNode::from("asdf")),
                    op: BinaryOperation::Add,
                    span: None,
                });

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::String(false));
            }

            #[test]
            fn string_op_int_is_string() {
                let context = CompilationContext::default();

                let node = ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from("asdf")),
                    r: Box::new(ExpressionNode::from(23423)),
                    op: BinaryOperation::Div,
                    span: None,
                });

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::String(false));
            }

            #[test]
            fn comma_expression_is_last_item() {
                let context = CompilationContext::default();

                let node = ExpressionNode::CommaExpression(CommaExpressionNode {
                    value: vec![ExpressionNode::from(123), ExpressionNode::from("foobar")],
                    span: None,
                });

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::String(false));
            }

            #[test]
            fn call_falls_back_to_efun_check() {
                let context = CompilationContext::default();

                let node = ExpressionNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![ExpressionNode::from("foo/bar.c")],
                    name: "clone_object".to_string(),
                    span: None,
                    namespace: CallNamespace::default(),
                });

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Object(false));
            }

            #[test]
            fn call_uses_namespace_to_get_correct_function() {
                let mut context = CompilationContext::default();

                let proto = FunctionPrototype::new(
                    "clone_object",
                    LpcType::Int(true),
                    FunctionArity::default(),
                    FunctionFlags::default(),
                    None,
                    vec![],
                    vec![],
                );
                context
                    .function_prototypes
                    .insert("clone_object".into(), proto);

                let node = ExpressionNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![ExpressionNode::from("foo/bar.c")],
                    name: "clone_object".to_string(),
                    span: None,
                    namespace: CallNamespace::Named(EFUN.into()),
                });

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Object(false));
            }

            #[test]
            fn closure_uses_return_type() {
                let context = CompilationContext::default();

                let node = ExpressionNode::Closure(ClosureNode {
                    name: "closure-123".into(),
                    return_type: LpcType::Mapping(true),
                    parameters: None,
                    flags: Default::default(),
                    body: vec![],
                    span: None,
                    scope_id: None,
                });

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Mapping(true));
            }
        }

        mod binary_ops {
            use super::*;
            use crate::compiler::semantic::{scope_tree::ScopeTree};

            #[test]
            fn test_index_array_returns_singular_of_left_type() {
                let mut scope_tree = ScopeTree::default();
                let id = scope_tree.push_new();
                let scope = scope_tree.get_mut(id).unwrap();
                scope.insert(Symbol {
                    name: "foo".to_string(),
                    type_: LpcType::Int(true),
                    ..Default::default()
                });

                let l = ExpressionNode::Var(VarNode {
                    name: "foo".to_string(),
                    span: None,
                    global: true,
                    function_name: false,
                    external_capture: false,
                });
                let r = ExpressionNode::from(1);

                let node = ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(l),
                    r: Box::new(r),
                    op: BinaryOperation::Index,
                    span: None,
                });

                let context = CompilationContext {
                    scopes: scope_tree,
                    ..Default::default()
                };

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Int(false));
            }

            #[test]
            fn test_index_mapping_is_mixed() {
                let mut scope_tree = ScopeTree::default();
                let id = scope_tree.push_new();
                let scope = scope_tree.get_mut(id).unwrap();
                scope.insert(Symbol {
                    name: "foo".to_string(),
                    type_: LpcType::Mapping(false),
                    ..Default::default()
                });

                let l = ExpressionNode::Var(VarNode {
                    name: "foo".to_string(),
                    span: None,
                    global: true,
                    function_name: false,
                    external_capture: false,
                });
                let r = ExpressionNode::from(1);

                let node = ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(l),
                    r: Box::new(r),
                    op: BinaryOperation::Index,
                    span: None,
                });

                let context = CompilationContext {
                    scopes: scope_tree,
                    ..Default::default()
                };

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Mixed(false));
            }
        }

        mod calls {

            use super::*;
            use crate::compiler::semantic::{scope_tree::ScopeTree};

            #[test]
            fn is_return_type_for_normal_functions() {
                let node = ExpressionNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![],
                    name: "this_object".to_string(),
                    span: None,
                    namespace: Default::default(),
                });

                let context = CompilationContext::default();

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Object(false));
            }

            #[test]
            fn is_mixed_for_call_other() {
                let node = ExpressionNode::Call(create!(
                    CallNode,
                    receiver: Some(Box::new(ExpressionNode::from(12345666))),
                ));

                let context = CompilationContext::default();

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Mixed(false));
            }

            #[test]
            fn is_mixed_for_function_pointers() {
                let mut scope_tree = ScopeTree::default();
                let id = scope_tree.push_new();
                let scope = scope_tree.get_mut(id).unwrap();
                scope.insert(Symbol {
                    name: "foo".to_string(),
                    type_: LpcType::Function(false),
                    ..Default::default()
                });

                let node = ExpressionNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![],
                    name: "foo".to_string(),
                    span: None,
                    namespace: Default::default(),
                });

                let context = CompilationContext {
                    scopes: scope_tree,
                    ..Default::default()
                };

                assert_eq!(node_type(&node, &context).unwrap(), LpcType::Mixed(false));
            }
        }
    }
}
