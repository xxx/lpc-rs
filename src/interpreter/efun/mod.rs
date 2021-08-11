mod clone_object;
mod debug;
mod dump;
mod file_name;
mod this_object;

use lazy_static::lazy_static;
use phf::phf_map;
use std::collections::HashMap;

use crate::{
    interpreter::asm_interpreter::AsmInterpreter,
    semantic::{
        function_flags::FunctionFlags, function_prototype::FunctionPrototype, lpc_type::LpcType,
    },
    Result,
};
use clone_object::clone_object;
use debug::debug;
use dump::dump;
use file_name::file_name;
use this_object::this_object;

/// Signature for Efuns
pub type Efun = fn(&mut AsmInterpreter) -> Result<()>;

pub const CALL_OTHER: &str = "call_other";
pub const CLONE_OBJECT: &str = "clone_object";
pub const DEBUG: &str = "debug";
pub const DUMP: &str = "dump";
pub const FILE_NAME: &str = "file_name";
pub const THIS_OBJECT: &str = "this_object";

/// Global static mapping of all efun names to the actual function
pub static EFUNS: phf::Map<&'static str, Efun> = phf_map! {
    // "call_other" is implemented with a custom [`Instruction`]
    "clone_object" => clone_object,
    "debug" => debug,
    "dump" => dump,
    "file_name" => file_name,
    "this_object" => this_object,
};

lazy_static! {
    /// Global static mapping of all efun names to their prototype
    pub static ref EFUN_PROTOTYPES: HashMap<&'static str, FunctionPrototype> = {
        let mut m = HashMap::new();

        m.insert(CALL_OTHER, FunctionPrototype {
            name: CALL_OTHER.into(),
            return_type: LpcType::Mixed(false),
            num_args: 2,
            num_default_args: 0,
            arg_types: vec![
                LpcType::Object(false)
                | LpcType::Object(true)
                | LpcType::String(false)
                | LpcType::String(true)
                | LpcType::Mapping(false),
                LpcType::String(false)
            ],
            span: None,
            arg_spans: vec![],
            flags: FunctionFlags::default().with_ellipsis(true),
        });

        m.insert(CLONE_OBJECT, FunctionPrototype {
            name: CLONE_OBJECT.into(),
            return_type: LpcType::Object(false),
            num_args: 1,
            num_default_args: 0,
            arg_types: vec![LpcType::String(false)],
            span: None,
            arg_spans: vec![],
            flags: FunctionFlags::default().with_ellipsis(false),
        });

        m.insert(DEBUG, FunctionPrototype {
            name: DEBUG.into(),
            return_type: LpcType::Mixed(false),
            num_args: 2,
            num_default_args: 1,
            arg_types: vec![LpcType::String(false), LpcType::Mixed(false)],
            span: None,
            arg_spans: vec![],
            flags: FunctionFlags::default().with_ellipsis(false),
        });

        m.insert(DUMP, FunctionPrototype {
            name: DUMP.into(),
            return_type: LpcType::Void,
            num_args: 1,
            num_default_args: 0,
            arg_types: vec![LpcType::Mixed(false)],
            span: None,
            arg_spans: vec![],
            flags: FunctionFlags::default().with_ellipsis(false),
        });

        m.insert(FILE_NAME, FunctionPrototype {
            name: FILE_NAME.into(),
            return_type: LpcType::String(false),
            num_args: 1,
            num_default_args: 0,
            arg_types: vec![LpcType::Object(false)],
            span: None,
            arg_spans: vec![],
            flags: FunctionFlags::default().with_ellipsis(false),
        });

        m.insert(THIS_OBJECT, FunctionPrototype {
            name: THIS_OBJECT.into(),
            return_type: LpcType::Object(false),
            num_args: 0,
            num_default_args: 0,
            arg_types: vec![],
            span: None,
            arg_spans: vec![],
            flags: FunctionFlags::default().with_ellipsis(false),
        });

        m
    };
}
