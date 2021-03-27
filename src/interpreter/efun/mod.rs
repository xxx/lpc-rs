mod dump;

use lazy_static::lazy_static;
use std::collections::HashMap;

use crate::{
    interpreter::{asm_interpreter::AsmInterpreter, stack_frame::StackFrame},
    semantic::{function_prototype::FunctionPrototype, lpc_type::LpcType},
};
use dump::dump;

/// Signature for Efuns
pub type Efun = fn(&StackFrame, &AsmInterpreter);

lazy_static! {
    /// Global static mapping of all efun names to the actual function
    pub static ref EFUNS: HashMap<&'static str, Efun> = {
        let mut m: HashMap<&str, Efun> = HashMap::new();
        m.insert("dump", dump);
        m
    };

    /// Global static mapping of all efun names to their prototype
    pub static ref EFUN_PROTOTYPES: HashMap<&'static str, FunctionPrototype> = {
        let mut m = HashMap::new();
        m.insert("dump", FunctionPrototype {
            name: String::from("dump"),
            return_type: LpcType::Int(false),
            num_args: 1,
            num_default_args: 0,
            arg_types: vec![LpcType::Mixed(false)],
            span: None,
            arg_spans: vec![]
        });
        m
    };
}
