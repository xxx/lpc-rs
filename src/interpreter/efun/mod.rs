mod print;

use std::collections::HashMap;
use lazy_static::lazy_static;

use print::print;
use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::asm_interpreter::AsmInterpreter;

/// Signature for Efuns
pub type Efun = fn(&StackFrame, &AsmInterpreter);

lazy_static! {
    /// Global static mapping of all efuns
    pub static ref EFUNS: HashMap<String, Efun> = {
        let mut m: HashMap<String, Efun> = HashMap::new();
        m.insert(String::from("print"), print);
        m
    };
}