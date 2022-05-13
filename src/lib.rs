pub mod asm;
pub mod ast;
pub mod codegen;
pub mod compilation_context;
pub mod compiler;
pub mod core;
pub mod errors;
pub mod interpreter;
pub mod parser;
pub mod preprocessor;
pub mod semantic;
pub mod util;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(#[allow(clippy::all)] pub lpc_parser);
lalrpop_mod!(#[allow(clippy::all)] pub preprocessor_parser);

extern crate serde;
#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate educe;

use crate::errors::LpcError;
use decorum::Total;
use std::result;

/// Abstracted type to use as in-game `int`s
pub type LpcInt = i64;

/// The base float-type that in-game `float`s are backed by
pub type BaseFloat = f64;

/// Abstracted type for in-game `float`s. The wrapper is to handle hashing, ordering, etc.
pub type LpcFloat = Total<BaseFloat>;

/// Common `Result` type
pub type Result<T> = result::Result<T, LpcError>;

/// Convert various literal escapes to actual escape characters
///
/// # Arguments
///
/// `s` - Something that can be represented as a `&str` through `as_ref`.
pub fn convert_escapes<T>(s: T) -> String
where
    T: AsRef<str>,
{
    s.as_ref()
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t")
        .replace("\\v", "\x0F")
        .replace("\\f", "\x0C")
        .replace("\\a", "\x07")
        .replace("\\b", "\x08")
}
