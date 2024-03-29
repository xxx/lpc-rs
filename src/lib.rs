#![forbid(unsafe_code)]
#![warn(clippy::disallowed_types)]

extern crate core;

use lalrpop_util::lalrpop_mod;

pub mod compile_time_config;
pub mod compiler;
pub mod interpreter;
pub mod telnet;
pub mod util;

#[cfg(test)]
pub mod test_support;

#[cfg(not(target_env = "msvc"))]
use jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

lalrpop_mod!(#[allow(clippy::all)] pub lpc_parser);
lalrpop_mod!(#[allow(clippy::all)] pub preprocessor_parser);
