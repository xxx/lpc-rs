pub mod ast;
pub mod asm;
pub mod codegen;
pub mod interpreter;
pub mod semantic;

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(#[allow(clippy::all)] pub mathstack_parser);