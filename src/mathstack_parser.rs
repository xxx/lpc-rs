use pest_consume::{Parser, Error};
use pest_consume::match_nodes;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

use crate::ast::expression_node::ExpressionNode;
use crate::ast::int_node::IntNode;
use crate::ast::ast_node::ASTNode;
use crate::ast::program_node::ProgramNode;
use crate::ast::binary_op_node::{BinaryOpNode, BinaryOperation};

#[derive(Parser, Debug)]
#[grammar = "mathstack.pest"]
pub struct MathstackParser;

#[pest_consume::parser]
impl MathstackParser {
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }
    fn program(input: Node) -> Result<ProgramNode> {
        Ok(match_nodes!(input.into_children();
            [expression(exps).., _] => ProgramNode {
                expressions: exps.collect(),
            }
        ))
    }
    fn expression(input: Node) -> Result<ExpressionNode> {
        Ok(match_nodes!(input.into_children();
            [term(a), binary_op(op), expression(b)] => BinaryOpNode {
                l: Box::new(a.into()),
                r: Box::new(b.into()),
                op
            }.into(),
            [term(a)] => BinaryOpNode {
                l: Box::new(a.into()),
                r: Box::new(IntNode { value: 0 }.into()),
                op: BinaryOperation::Add
            }.into()
        ))
    }
    fn term(input: Node) -> Result<ASTNode> {
        Ok(match_nodes!(input.into_children();
            [int(a)] => a.into(),
            [expression(e)] => e.into()
        ))
    }
    fn binary_op(input: Node) -> Result<BinaryOperation> {
        Ok(match_nodes!(input.into_children();
            [add_op(a)] => a,
            [sub_op(a)] => a,
            [mul_op(a)] => a,
            [div_op(a)] => a,
        ))
    }
    fn add_op(input: Node) -> Result<BinaryOperation> {
        Ok(BinaryOperation::Add)
    }
    fn sub_op(input: Node) -> Result<BinaryOperation> {
        Ok(BinaryOperation::Sub)
    }
    fn mul_op(input: Node) -> Result<BinaryOperation> {
        Ok(BinaryOperation::Mul)
    }
    fn div_op(input: Node) -> Result<BinaryOperation> {
        Ok(BinaryOperation::Div)
    }
    fn int(input: Node) -> Result<IntNode> {
        match input.as_str().parse::<i64>() {
            Ok(value) => Ok(IntNode { value }),
            Err(e) => Err(input.error(e))
        }
    }
}

pub fn parse_program(input_str: &str) -> Result<ProgramNode> {
    let inputs = MathstackParser::parse(Rule::program, input_str)?;
    let input = inputs.single()?;
    // Consume the `Node` recursively into the final value
    MathstackParser::program(input)
}
