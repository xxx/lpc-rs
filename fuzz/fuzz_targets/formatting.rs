#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| lpc_rs::fuzzing::formatting(data));
