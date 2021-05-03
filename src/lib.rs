pub mod asm;
pub mod ast;
pub mod codegen;
pub mod compiler;
pub mod context;
pub mod errors;
pub mod interpreter;
pub mod parser;
pub mod preprocessor;
pub mod semantic;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(#[allow(clippy::all)] pub lpc_parser);
lalrpop_mod!(#[allow(clippy::all)] pub preprocessor_parser);

extern crate serde;
#[macro_use]
extern crate serde_derive;

use decorum::Total;

pub type LpcInt = i64;
pub type LpcFloat = Total<f64>;

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
