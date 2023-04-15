pub mod efun_context;

pub(crate) mod call_out;
pub(crate) mod clone_object;
pub(crate) mod compose;
pub(crate) mod debug;
pub(crate) mod dump;
pub(crate) mod file_name;
pub(crate) mod find_object;
pub(crate) mod papplyv;
pub(crate) mod query_call_out;
pub(crate) mod query_call_outs;
pub(crate) mod remove_call_out;
pub(crate) mod this_object;
pub(crate) mod throw;

use futures::future::BoxFuture;
use indexmap::IndexMap;
use lpc_rs_core::{
    function_arity::{FunctionArity, FunctionArityBuilder},
    function_flags::FunctionFlags,
    lpc_path::LpcPath,
    lpc_type::LpcType,
};
use lpc_rs_errors::Result;
use lpc_rs_function_support::function_prototype::{
    FunctionKind, FunctionPrototype, FunctionPrototypeBuilder,
};
use once_cell::sync::Lazy;

use crate::interpreter::efun::efun_context::EfunContext;

/// Signature for Efuns
pub type Efun<const N: usize> = fn(&mut EfunContext<N>) -> Result<()>;
pub type AsyncEfun<const N: usize> =
    Box<dyn Send + Sync + for<'a> Fn(&'a mut EfunContext<N>) -> BoxFuture<'a, Result<()>>>;

pub const CALL_OUT: &str = "call_out";
pub const CALL_OTHER: &str = "call_other";
pub const CATCH: &str = "catch";
pub const CLONE_OBJECT: &str = "clone_object";
pub const COMPOSE: &str = "compose";
pub const DEBUG: &str = "debug";
pub const DUMP: &str = "dump";
pub const FILE_NAME: &str = "file_name";
pub const FIND_OBJECT: &str = "find_object";
pub const PAPPLYV: &str = "papplyv";
pub const QUERY_CALL_OUT: &str = "query_call_out";
pub const QUERY_CALL_OUTS: &str = "query_call_outs";
pub const REMOVE_CALL_OUT: &str = "remove_call_out";
pub const SIZEOF: &str = "sizeof";
pub const THIS_OBJECT: &str = "this_object";
pub const THROW: &str = "throw";

pub async fn call_efun<const STACKSIZE: usize>(
    efun_name: &str,
    efun_context: &mut EfunContext<'_, STACKSIZE>,
) -> Result<()> {
    match efun_name {
        CALL_OUT => call_out::call_out(efun_context).await,
        CLONE_OBJECT => clone_object::clone_object(efun_context).await,
        COMPOSE => compose::compose(efun_context).await,
        DEBUG => debug::debug(efun_context).await,
        DUMP => dump::dump(efun_context).await,
        FILE_NAME => file_name::file_name(efun_context).await,
        FIND_OBJECT => find_object::find_object(efun_context).await,
        PAPPLYV => papplyv::papplyv(efun_context).await,
        QUERY_CALL_OUT => query_call_out::query_call_out(efun_context).await,
        QUERY_CALL_OUTS => query_call_outs::query_call_outs(efun_context).await,
        REMOVE_CALL_OUT => remove_call_out::remove_call_out(efun_context).await,
        THIS_OBJECT => this_object::this_object(efun_context).await,
        THROW => throw::throw(efun_context).await,
        _ => Err(efun_context.runtime_error(format!("Unknown efun: {}", efun_name))),
    }
}

/// Global static mapping of all efun names to their prototype.
/// [`Instruction::CallEfun`](lpc_rs_asm::instruction::Instruction::CallEfun) indexes into this map,
/// so changes to the insert order will invalidate previously-compiled code, and break tests.
pub static EFUN_PROTOTYPES: Lazy<IndexMap<&'static str, FunctionPrototype>> = Lazy::new(|| {
    let mut m = IndexMap::new();

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
        COMPOSE,
        FunctionPrototypeBuilder::default()
            .name(COMPOSE)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Function(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(2))
            .arg_types(vec![LpcType::Function(false), LpcType::Function(false)])
            .build()
            .expect("failed to build compose"),
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
        FIND_OBJECT,
        FunctionPrototypeBuilder::default()
            .name(FIND_OBJECT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::String(false)])
            .build()
            .expect("failed to build find_object"),
    );

    m.insert(
        PAPPLYV,
        FunctionPrototypeBuilder::default()
            .name(PAPPLYV)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Function(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 2,
                num_default_args: 0,
                varargs: false,
                ellipsis: false,
            })
            .arg_types(vec![LpcType::Function(false), LpcType::Mixed(true)])
            .build()
            .expect("failed to build papplyv"),
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
        QUERY_CALL_OUTS,
        FunctionPrototypeBuilder::default()
            .name(QUERY_CALL_OUTS)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Mixed(true))
            .kind(FunctionKind::Efun)
            .arity(
                FunctionArityBuilder::default()
                    .num_args(1)
                    .num_default_args(1)
                    .build()
                    .unwrap(),
            )
            .arg_types(vec![LpcType::Object(false)])
            .build()
            .expect("failed to build query_call_out_info"),
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
