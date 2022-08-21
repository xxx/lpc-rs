use lalrpop_util::lalrpop_mod;

pub mod compile_time_config;
pub mod compiler;
pub mod interpreter;
pub mod util;

#[cfg(test)]
pub mod test_support;

lalrpop_mod!(#[allow(clippy::all)] pub lpc_parser);
lalrpop_mod!(#[allow(clippy::all)] pub preprocessor_parser);
