#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|text: &str| lpc_rs::fuzzing::save_format(text));
