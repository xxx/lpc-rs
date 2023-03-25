pub mod efun_context;

pub(crate) mod clone_object;
pub(crate) mod debug;
pub(crate) mod dump;
pub(crate) mod file_name;
pub(crate) mod this_object;
pub(crate) mod throw;

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
use phf::phf_map;
use qcell::QCellOwner;
use this_object::this_object;
use throw::throw;

use crate::interpreter::efun::efun_context::EfunContext;

/// Signature for Efuns
pub type Efun<const N: usize> = fn(&mut EfunContext<N>, cell_key: &mut QCellOwner) -> Result<()>;

pub const CALL_OTHER: &str = "call_other";
pub const CATCH: &str = "catch";
pub const CLONE_OBJECT: &str = "clone_object";
pub const DEBUG: &str = "debug";
pub const DUMP: &str = "dump";
pub const FILE_NAME: &str = "file_name";
pub const SIZEOF: &str = "sizeof";
pub const THIS_OBJECT: &str = "this_object";
pub const THROW: &str = "throw";

/// A blanket to get a compile-time constant map of efuns regardless of stack size.
pub trait HasEfuns<const STACKSIZE: usize> {
    const EFUNS: phf::Map<&'static str, Efun<STACKSIZE>> = phf_map! {
        "clone_object" => clone_object::clone_object as Efun<STACKSIZE>,
        "debug" => debug::debug as Efun<STACKSIZE>,
        "dump" => dump::dump as Efun<STACKSIZE>,
        "file_name" => file_name::file_name as Efun<STACKSIZE>,
        "this_object" => this_object::this_object as Efun<STACKSIZE>,
        "throw" => throw::throw as Efun<STACKSIZE>,
    };
}

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
    );

    m.insert(
        THIS_OBJECT,
        FunctionPrototypeBuilder::default()
            .name(THIS_OBJECT)
            .return_type(LpcType::Object(false))
            .build()
            .expect("failed to build this_object"),
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
pub fn call_efun<const N: usize>(
    name: &str,
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    match name {
        CLONE_OBJECT => clone_object(context, cell_key),
        DEBUG => debug(context, cell_key),
        DUMP => dump(context, cell_key),
        FILE_NAME => file_name(context, cell_key),
        THIS_OBJECT => this_object(context, cell_key),
        THROW => throw(context, cell_key),
        _ => Err(LpcError::new(format!(
            "runtime error: call to unknown function (that had a valid prototype?) `{name}`"
        ))),
    }
}
