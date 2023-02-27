pub mod efun_context;

mod clone_object;
mod debug;
mod dump;
mod file_name;
mod this_object;
mod throw;

use std::collections::HashMap;

use clone_object::clone_object;
use debug::debug;
use dump::dump;
use file_name::file_name;
use lpc_rs_core::{
    function_arity::FunctionArity, function_flags::FunctionFlags, lpc_type::LpcType,
};
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::function_prototype::{FunctionPrototype, FunctionPrototypeBuilder};
use once_cell::sync::Lazy;
use this_object::this_object;
use throw::throw;

use crate::interpreter::efun::efun_context::EfunContext;

/// Signature for Efuns
pub type Efun<const N: usize> = fn(&mut EfunContext<N>) -> Result<()>;

pub const CALL_OTHER: &str = "call_other";
pub const CATCH: &str = "catch";
pub const CLONE_OBJECT: &str = "clone_object";
pub const DEBUG: &str = "debug";
pub const DUMP: &str = "dump";
pub const FILE_NAME: &str = "file_name";
pub const SIZEOF: &str = "sizeof";
pub const THIS_OBJECT: &str = "this_object";
pub const THROW: &str = "throw";

/// Global static mapping of all efun names to their prototype
pub static EFUN_PROTOTYPES: Lazy<HashMap<&'static str, FunctionPrototype>> = Lazy::new(|| {
    let mut m = HashMap::new();

    m.insert(
        CALL_OTHER,
        FunctionPrototypeBuilder::default()
            .name(CALL_OTHER)
            .return_type(LpcType::Mixed(false))
            .arity(FunctionArity {
                num_args: 2,
                num_default_args: 0,
                varargs: false,
                ellipsis: true,
            })
            .arg_types(vec![
                LpcType::Object(false)
                    | LpcType::Object(true)
                    | LpcType::String(false)
                    | LpcType::String(true)
                    | LpcType::Mapping(false),
                LpcType::String(false),
            ])
            .flags(FunctionFlags::default().with_ellipsis(true))
            .build()
            .expect("failed to build call_other"),
        // FunctionPrototype {
        //     name: CALL_OTHER.into(),
        //     return_type: LpcType::Mixed(false),
        //     arity: FunctionArity {
        //         num_args: 2,
        //         num_default_args: 0,
        //         varargs: false,
        //         ellipsis: true,
        //     },
        //     arg_types: vec![
        //         LpcType::Object(false)
        //             | LpcType::Object(true)
        //             | LpcType::String(false)
        //             | LpcType::String(true)
        //             | LpcType::Mapping(false),
        //         LpcType::String(false),
        //     ],
        //     span: None,
        //     arg_spans: vec![],
        //     flags: FunctionFlags::default().with_ellipsis(true),
        // },
    );

    // "catch" is a special form of the language, implemented with custom
    // [`Instruction`]s.   A prototype is defined here to enforce type checks,
    //   as `catch` looks and acts like a function call.
    m.insert(
        CATCH,
        FunctionPrototypeBuilder::default()
            .name(CATCH)
            .return_type(LpcType::Mixed(false))
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Mixed(false) | LpcType::Void])
            .build()
            .expect("failed to build catch"),
        // FunctionPrototype {
        //     name: CATCH.into(),
        //     return_type: LpcType::Mixed(false),
        //     arity: FunctionArity::new(1),
        //     arg_types: vec![LpcType::Mixed(false) | LpcType::Void],
        //     span: None,
        //     arg_spans: vec![],
        //     flags: FunctionFlags::default().with_ellipsis(false),
        // },
    );

    m.insert(
        CLONE_OBJECT,
        FunctionPrototypeBuilder::default()
            .name(CLONE_OBJECT)
            .return_type(LpcType::Object(false))
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::String(false)])
            .build()
            .expect("failed to build clone_object"),
        // FunctionPrototype {
        //     name: CLONE_OBJECT.into(),
        //     return_type: LpcType::Object(false),
        //     arity: FunctionArity::new(1),
        //     arg_types: vec![LpcType::String(false)],
        //     span: None,
        //     arg_spans: vec![],
        //     flags: FunctionFlags::default().with_ellipsis(false),
        // },
    );

    m.insert(
        DEBUG,
        FunctionPrototypeBuilder::default()
            .name(DEBUG)
            .return_type(LpcType::Mixed(false))
            .arity(FunctionArity {
                num_args: 2,
                num_default_args: 1,
                varargs: false,
                ellipsis: false,
            })
            .arg_types(vec![LpcType::String(false), LpcType::Mixed(false)])
            .build()
            .expect("failed to build debug"),
        // FunctionPrototype {
        //     name: DEBUG.into(),
        //     return_type: LpcType::Mixed(false),
        //     arity: FunctionArity {
        //         num_args: 2,
        //         num_default_args: 1,
        //         ellipsis: false,
        //         varargs: false,
        //     },
        //     arg_types: vec![LpcType::String(false), LpcType::Mixed(false)],
        //     span: None,
        //     arg_spans: vec![],
        //     flags: FunctionFlags::default().with_ellipsis(false),
        // },
    );

    m.insert(
        DUMP,
        FunctionPrototypeBuilder::default()
            .name(DUMP)
            .return_type(LpcType::Void)
            .arity(FunctionArity {
                num_args: 1,
                num_default_args: 0,
                varargs: false,
                ellipsis: true,
            })
            .arg_types(vec![LpcType::Mixed(false)])
            .flags(FunctionFlags::default().with_ellipsis(true))
            .build()
            .expect("failed to build dump"),
        // FunctionPrototype {
        //     name: DUMP.into(),
        //     return_type: LpcType::Void,
        //     arity: FunctionArity {
        //         num_args: 1,
        //         ellipsis: true,
        //         num_default_args: 0,
        //         varargs: false,
        //     },
        //     arg_types: vec![LpcType::Mixed(false)],
        //     span: None,
        //     arg_spans: vec![],
        //     flags: FunctionFlags::default().with_ellipsis(true),
        // },
    );

    m.insert(
        FILE_NAME,
        FunctionPrototypeBuilder::default()
            .name(FILE_NAME)
            .return_type(LpcType::String(false))
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Object(false)])
            .build()
            .expect("failed to build file_name"),
        // FunctionPrototype {
        //     name: FILE_NAME.into(),
        //     return_type: LpcType::String(false),
        //     arity: FunctionArity::new(1),
        //     arg_types: vec![LpcType::Object(false)],
        //     span: None,
        //     arg_spans: vec![],
        //     flags: FunctionFlags::default().with_ellipsis(false),
        // },
    );

    // sizeof is handled with its own instruction, but is typechecked as normal
    m.insert(
        SIZEOF,
        FunctionPrototypeBuilder::default()
            .name(SIZEOF)
            .return_type(LpcType::Int(false))
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Mixed(true) | LpcType::Mapping(false)])
            .build()
            .expect("failed to build sizeof"),
        // FunctionPrototype {
        //     name: SIZEOF.into(),
        //     return_type: LpcType::Int(false),
        //     arity: FunctionArity::new(1),
        //     arg_types: vec![LpcType::Mixed(true) | LpcType::Mapping(false)],
        //     span: None,
        //     arg_spans: vec![],
        //     flags: FunctionFlags::default().with_ellipsis(false),
        // },
    );

    m.insert(
        THIS_OBJECT,
        FunctionPrototypeBuilder::default()
            .name(THIS_OBJECT)
            .return_type(LpcType::Object(false))
            .build()
            .expect("failed to build this_object"),
        // FunctionPrototype {
        //     name: THIS_OBJECT.into(),
        //     return_type: LpcType::Object(false),
        //     arity: FunctionArity::default(),
        //     arg_types: vec![],
        //     span: None,
        //     arg_spans: vec![],
        //     flags: FunctionFlags::default().with_ellipsis(false),
        // },
    );

    m.insert(
        THROW,
        FunctionPrototypeBuilder::default()
            .name(THROW)
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Mixed(false)])
            .build()
            .expect("failed to build throw"),
    );

    m
});

/// call the actual function, from the given name, with the passed context.
pub fn call_efun<const N: usize>(name: &str, context: &mut EfunContext<N>) -> Result<()> {
    match name {
        CLONE_OBJECT => clone_object(context),
        DEBUG => debug(context),
        DUMP => dump(context),
        FILE_NAME => file_name(context),
        THIS_OBJECT => this_object(context),
        THROW => throw(context),
        _ => Err(LpcError::new(format!(
            "runtime error: call to unknown function (that had a valid prototype?) `{name}`"
        ))),
    }
}
