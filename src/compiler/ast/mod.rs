pub mod array_node;
pub mod assignment_node;
pub mod ast_node;
pub mod binary_op_node;
pub mod block_node;
pub mod break_node;
pub mod call_node;
pub mod capture_node;
pub mod cast_node;
pub mod catch_node;
pub mod closure_node;
pub mod comma_expression_node;
pub mod continue_node;
pub mod decl_node;
pub mod do_while_node;
pub mod expression_node;
pub mod float_node;
pub mod for_each_node;
pub mod for_node;
pub mod function_def_node;
pub mod function_ptr_node;
pub mod if_node;
pub mod inherit_node;
pub mod int_node;
pub mod label_node;
pub mod mapping_node;
pub mod nil_node;
pub mod operator_node;
pub mod program_node;
pub mod range_node;
pub mod ref_node;
pub mod return_node;
pub mod spread_node;
pub mod string_node;
pub mod switch_node;
pub mod ternary_node;
pub mod unary_op_node;
pub mod var_init_node;
pub mod var_node;
pub mod while_node;

pub mod class_def_node;
pub mod labeled_statement_node;
pub mod parsed_type;

pub mod class_expression;

#[cfg(test)]
mod tests {
    use super::{
        ast_node::AstNode, do_while_node::DoWhileNode, expression_node::ExpressionNode,
        if_node::IfNode, int_node::IntNode, switch_node::SwitchNode, while_node::WhileNode,
    };

    #[test]
    fn control_flow_formatting_balances_delimiters() {
        let condition = || ExpressionNode::Int(IntNode::new(1));
        let body = || AstNode::NoOp;
        let nodes = [
            DoWhileNode::new(body(), condition(), None).to_string(),
            IfNode::new(condition(), body(), None, None).to_string(),
            IfNode::new(condition(), body(), Some(body()), None).to_string(),
            SwitchNode::new(condition(), body(), None).to_string(),
            WhileNode::new(condition(), body(), None).to_string(),
        ];

        assert_eq!(
            nodes,
            [
                "do { NoOp } while (1)",
                "if(1) { NoOp }",
                "if(1) { NoOp } else { NoOp }",
                "switch(1) { NoOp }",
                "while(1) { NoOp }",
            ]
        );
    }
}
