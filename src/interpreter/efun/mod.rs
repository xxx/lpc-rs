pub mod efun_context;

pub(crate) mod call_out;
pub(crate) mod clone_object;
pub(crate) mod debug;
pub(crate) mod dump;
pub(crate) mod file_name;
pub(crate) mod query_call_out;
pub(crate) mod remove_call_out;
pub(crate) mod this_object;
pub(crate) mod throw;

use std::collections::HashMap;

use lpc_rs_core::{
    function_arity::FunctionArity, function_flags::FunctionFlags, lpc_path::LpcPath,
    lpc_type::LpcType,
};
use lpc_rs_errors::Result;
use lpc_rs_function_support::function_prototype::{
    FunctionKind, FunctionPrototype, FunctionPrototypeBuilder,
};
use once_cell::sync::Lazy;
use phf::phf_map;
use qcell::QCellOwner;

use crate::interpreter::efun::efun_context::EfunContext;

/// Signature for Efuns
pub type Efun<const N: usize> = fn(&mut EfunContext<N>, cell_key: &mut QCellOwner) -> Result<()>;

pub const CALL_OUT: &str = "call_out";
pub const CALL_OTHER: &str = "call_other";
pub const CATCH: &str = "catch";
pub const CLONE_OBJECT: &str = "clone_object";
pub const DEBUG: &str = "debug";
pub const DUMP: &str = "dump";
pub const FILE_NAME: &str = "file_name";
pub const QUERY_CALL_OUT: &str = "query_call_out";
pub const REMOVE_CALL_OUT: &str = "remove_call_out";
pub const SIZEOF: &str = "sizeof";
pub const THIS_OBJECT: &str = "this_object";
pub const THROW: &str = "throw";

/// A blanket to get a compile-time constant map of efuns regardless of stack size.
pub trait HasEfuns<const STACKSIZE: usize> {
    const EFUNS: phf::Map<&'static str, Efun<STACKSIZE>> = phf_map! {
        "call_out" => call_out::call_out as Efun<STACKSIZE>,
        "clone_object" => clone_object::clone_object as Efun<STACKSIZE>,
        "debug" => debug::debug as Efun<STACKSIZE>,
        "dump" => dump::dump as Efun<STACKSIZE>,
        "file_name" => file_name::file_name as Efun<STACKSIZE>,
        "query_call_out" => query_call_out::query_call_out as Efun<STACKSIZE>,
        "remove_call_out" => remove_call_out::remove_call_out as Efun<STACKSIZE>,
        "this_object" => this_object::this_object as Efun<STACKSIZE>,
        "throw" => throw::throw as Efun<STACKSIZE>,
    };
}

/// Global static mapping of all efun names to their prototype
pub static EFUN_PROTOTYPES: Lazy<HashMap<&'static str, FunctionPrototype>> = Lazy::new(|| {
    let mut m = HashMap::new();

    m.insert(
        CALL_OUT,
        FunctionPrototypeBuilder::default()
            .name(CALL_OUT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 2,
                num_default_args: 0,
                varargs: false,
                ellipsis: true,
            })
            .arg_types(vec![
                LpcType::Function(false),
                LpcType::Int(false) | LpcType::Float(false),
            ])
            .flags(FunctionFlags::default().with_ellipsis(false))
            .build()
            .expect("failed to build call_out"),
    );
    m.insert(
        CALL_OTHER,
        FunctionPrototypeBuilder::default()
            .name(CALL_OTHER)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Mixed(false))
            .kind(FunctionKind::Efun)
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
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Mixed(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Mixed(false) | LpcType::Void])
            .build()
            .expect("failed to build catch"),
    );

    m.insert(
        CLONE_OBJECT,
        FunctionPrototypeBuilder::default()
            .name(CLONE_OBJECT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::String(false)])
            .build()
            .expect("failed to build clone_object"),
    );

    m.insert(
        DEBUG,
        FunctionPrototypeBuilder::default()
            .name(DEBUG)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Mixed(false))
            .kind(FunctionKind::Efun)
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
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Void)
            .kind(FunctionKind::Efun)
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
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::String(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Object(false)])
            .build()
            .expect("failed to build file_name"),
    );

    m.insert(
        QUERY_CALL_OUT,
        FunctionPrototypeBuilder::default()
            .name(QUERY_CALL_OUT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Mixed(true))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Int(false)])
            .build()
            .expect("failed to build query_call_out"),
    );

    m.insert(
        REMOVE_CALL_OUT,
        FunctionPrototypeBuilder::default()
            .name(REMOVE_CALL_OUT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Int(false)])
            .build()
            .expect("failed to build remove_call_out"),
    );

    // sizeof is handled with its own instruction, but is typechecked as normal
    m.insert(
        SIZEOF,
        FunctionPrototypeBuilder::default()
            .name(SIZEOF)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Mixed(true) | LpcType::Mapping(false)])
            .build()
            .expect("failed to build sizeof"),
    );

    m.insert(
        THIS_OBJECT,
        FunctionPrototypeBuilder::default()
            .name(THIS_OBJECT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(false))
            .kind(FunctionKind::Efun)
            .build()
            .expect("failed to build this_object"),
    );

    m.insert(
        THROW,
        FunctionPrototypeBuilder::default()
            .name(THROW)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Void)
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Mixed(false)])
            .build()
            .expect("failed to build throw"),
    );

    m
});
