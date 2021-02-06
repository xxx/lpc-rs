mod print;

use std::collections::HashMap;
use lazy_static::lazy_static;

use print::print;
use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::asm_interpreter::AsmInterpreter;
use crate::semantic::function_prototype::FunctionPrototype;
use crate::semantic::lpc_type::LPCType;

/// Signature for Efuns
pub type Efun = fn(&StackFrame, &AsmInterpreter);

lazy_static! {
    /// Global static mapping of all efun names to the actual function
    pub static ref EFUNS: HashMap<&'static str, Efun> = {
        let mut m: HashMap<&str, Efun> = HashMap::new();
        m.insert("print", print);
        m
    };

    /// Global static mapping of all efun names to their prototype
    pub static ref EFUN_PROTOTYPES: HashMap<&'static str, FunctionPrototype> = {
        let mut m = HashMap::new();
        m.insert("print", FunctionPrototype {
            name: String::from("print"),
            return_type: LPCType::Int(false),
            num_args: 1,
            arg_types: vec![LPCType::Int(false) | LPCType::String(false) | LPCType::Int(true)],
            span: None,
            arg_spans: vec![]
        });
        m
    };
}