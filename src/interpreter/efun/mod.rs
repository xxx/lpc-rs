mod clone_object;
mod dump;
mod this_object;

use lazy_static::lazy_static;
use phf::phf_map;
use std::collections::HashMap;

use crate::{
    interpreter::asm_interpreter::AsmInterpreter,
    semantic::{function_prototype::FunctionPrototype, lpc_type::LpcType},
    Result,
};
use clone_object::clone_object;
use dump::dump;
use this_object::this_object;

/// Signature for Efuns
pub type Efun = fn(&mut AsmInterpreter) -> Result<()>;

pub const CLONE_OBJECT: &str = "clone_object";
pub const DUMP: &str = "dump";
pub const THIS_OBJECT: &str = "this_object";

/// Global static mapping of all efun names to the actual function
pub static EFUNS: phf::Map<&'static str, Efun> = phf_map! {
    "clone_object" => clone_object,
    "dump" => dump,
    "this_object" => this_object,
};

lazy_static! {
    /// Global static mapping of all efun names to their prototype
    pub static ref EFUN_PROTOTYPES: HashMap<&'static str, FunctionPrototype> = {
        let mut m = HashMap::new();
        m.insert(CLONE_OBJECT, FunctionPrototype {
            name: String::from(CLONE_OBJECT),
            return_type: LpcType::Object(false),
            num_args: 1,
            num_default_args: 0,
            arg_types: vec![LpcType::String(false)],
            span: None,
            arg_spans: vec![]
        });

        m.insert(DUMP, FunctionPrototype {
            name: String::from(DUMP),
            return_type: LpcType::Void,
            num_args: 1,
            num_default_args: 0,
            arg_types: vec![LpcType::Mixed(false)],
            span: None,
            arg_spans: vec![]
        });

        m.insert(THIS_OBJECT, FunctionPrototype {
            name: String::from(THIS_OBJECT),
            return_type: LpcType::Object(false),
            num_args: 0,
            num_default_args: 0,
            arg_types: vec![],
            span: None,
            arg_spans: vec![]
        });

        m
    };
}
