pub mod asm;
pub mod ast;
pub mod codegen;
pub mod errors;
pub mod interpreter;
pub mod parser;
pub mod semantic;
pub mod compiler;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(#[allow(clippy::all)] pub lpc_parser);

extern crate serde;
#[macro_use]
extern crate serde_derive;
