pub mod asm;
pub mod ast;
pub mod codegen;
pub mod compilation_context;
pub mod compile_time_config;
pub mod compiler;
pub mod core;
pub mod errors;
pub mod interpreter;
pub mod parser;
pub mod preprocessor;
pub mod semantic;
pub mod util;

#[cfg(test)]
pub mod test_support;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(#[allow(clippy::all)] pub lpc_parser);
lalrpop_mod!(#[allow(clippy::all)] pub preprocessor_parser);

extern crate serde;

#[macro_use]
extern crate educe;

use crate::errors::LpcError;
use std::result;

/// Common `Result` type
pub type Result<T> = result::Result<T, LpcError>;
