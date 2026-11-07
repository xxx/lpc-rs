#![forbid(unsafe_code)]
#![cfg_attr(any(fuzzing, feature = "fuzzing"), recursion_limit = "256")]
#![warn(clippy::disallowed_types)]

extern crate core;

use lalrpop_util::lalrpop_mod;

pub mod command;
pub mod compile_time_config;
pub mod compiler;
pub mod interpreter;
pub mod runtime;
pub mod telnet;
pub mod util;

#[cfg(feature = "fuzzing")]
#[doc(hidden)]
pub mod fuzzing;

pub mod bench_support;

#[cfg(test)]
pub mod test_support;

#[cfg(all(not(target_env = "msvc"), not(fuzzing)))]
use jemallocator::Jemalloc;

#[cfg(all(not(target_env = "msvc"), not(fuzzing)))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

lalrpop_mod!(#[allow(clippy::all)] pub lpc_parser);
