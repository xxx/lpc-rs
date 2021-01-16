mod print;

use std::collections::HashMap;
use lazy_static::lazy_static;

use print::print;
use crate::interpreter::stack_frame::StackFrame;
use crate::asm::register::Register;

pub type Efun = fn(&StackFrame, &Register);

lazy_static! {
    pub static ref EFUNS: HashMap<String, Efun> = {
        let mut m: HashMap<String, Efun> = HashMap::new();
        m.insert(String::from("print"), print);
        m
    };
}