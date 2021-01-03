use core::fmt::Debug;
use enum_dispatch::enum_dispatch;
use crate::ast::{expression_node, int_node, program_node};
use expression_node::ExpressionNode;
use int_node::IntNode;
use program_node::ProgramNode;
use crate::codegen::tree_walker::TreeWalkerTrait;
use auto_impl::auto_impl;

#[enum_dispatch]
#[derive(Debug)]
pub enum ASTNode {
    ExpressionNode,
    IntNode,
    ProgramNode
}

#[auto_impl(&, &mut)]
#[enum_dispatch(ASTNode)]
pub trait ASTNodeTrait {
    fn to_str(&self) -> String;
    fn visit(&self, tree_walker: &mut impl TreeWalkerTrait);
}
