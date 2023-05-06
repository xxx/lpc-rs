pub mod efun_context;

pub(crate) mod all_environment;
pub(crate) mod all_inventory;
pub(crate) mod call_out;
pub(crate) mod clone_object;
pub(crate) mod compose;
pub(crate) mod debug;
pub(crate) mod deep_inventory;
pub(crate) mod destruct;
pub(crate) mod disable_commands;
pub(crate) mod dump;
pub(crate) mod enable_commands;
pub(crate) mod environment;
pub(crate) mod exec;
pub(crate) mod file_name;
pub(crate) mod find_object;
pub(crate) mod input_to;
pub(crate) mod interactive;
pub(crate) mod living;
pub(crate) mod move_object;
pub(crate) mod papplyv;
pub(crate) mod query_call_out;
pub(crate) mod query_call_outs;
pub(crate) mod query_resident_memory;
pub(crate) mod remove_call_out;
pub(crate) mod set_this_player;
pub(crate) mod tell_object;
pub(crate) mod this_object;
pub(crate) mod this_player;
pub(crate) mod throw;
pub(crate) mod write;
pub(crate) mod write_socket;

use std::sync::Arc;

use futures::future::BoxFuture;
use indexmap::IndexMap;
use lpc_rs_core::{
    function_arity::{FunctionArity, FunctionArityBuilder},
    function_flags::FunctionFlags,
    lpc_path::LpcPath,
    lpc_type::LpcType,
};
use lpc_rs_errors::Result;
use lpc_rs_function_support::{
    function_prototype::{FunctionKind, FunctionPrototype, FunctionPrototypeBuilder},
    program_function::ProgramFunction,
};
use once_cell::sync::Lazy;

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef, process::Process,
};

/// Signature for Efuns
pub type Efun<const N: usize> = fn(&mut EfunContext<N>) -> Result<()>;
pub type AsyncEfun<const N: usize> =
    Box<dyn Send + Sync + for<'a> Fn(&'a mut EfunContext<N>) -> BoxFuture<'a, Result<()>>>;

pub const ALL_ENVIRONMENT: &str = "all_environment";
pub const ALL_INVENTORY: &str = "all_inventory";
pub const CALL_OUT: &str = "call_out";
pub const CALL_OTHER: &str = "call_other";
pub const CATCH: &str = "catch";
pub const CLONE_OBJECT: &str = "clone_object";
pub const COMPOSE: &str = "compose";
pub const DEBUG: &str = "debug";
pub const DEEP_INVENTORY: &str = "deep_inventory";
pub const DESTRUCT: &str = "destruct";
pub const DISABLE_COMMANDS: &str = "disable_commands";
pub const DUMP: &str = "dump";
pub const ENABLE_COMMANDS: &str = "enable_commands";
pub const ENVIRONMENT: &str = "environment";
pub const EXEC: &str = "exec";
pub const FILE_NAME: &str = "file_name";
pub const FIND_OBJECT: &str = "find_object";
pub const INPUT_TO: &str = "input_to";
pub const INTERACTIVE: &str = "interactive";
pub const LIVING: &str = "living";
pub const MOVE_OBJECT: &str = "move_object";
pub const PAPPLYV: &str = "papplyv";
pub const QUERY_CALL_OUT: &str = "query_call_out";
pub const QUERY_CALL_OUTS: &str = "query_call_outs";
pub const QUERY_RESIDENT_MEMORY: &str = "query_resident_memory";
pub const REMOVE_CALL_OUT: &str = "remove_call_out";
pub const SET_THIS_PLAYER: &str = "set_this_player";
pub const SIZEOF: &str = "sizeof";
pub const TELL_OBJECT: &str = "tell_object";
pub const THIS_OBJECT: &str = "this_object";
pub const THIS_PLAYER: &str = "this_player";
pub const THROW: &str = "throw";
pub const WRITE: &str = "write";
pub const WRITE_SOCKET: &str = "write_socket";

pub async fn call_efun<const STACKSIZE: usize>(
    efun_name: &str,
    efun_context: &mut EfunContext<'_, STACKSIZE>,
) -> Result<()> {
    match efun_name {
        ALL_ENVIRONMENT => all_environment::all_environment(efun_context).await,
        ALL_INVENTORY => all_inventory::all_inventory(efun_context).await,
        CALL_OUT => call_out::call_out(efun_context).await,
        CLONE_OBJECT => clone_object::clone_object(efun_context).await,
        COMPOSE => compose::compose(efun_context).await,
        DEBUG => debug::debug(efun_context).await,
        DEEP_INVENTORY => deep_inventory::deep_inventory(efun_context).await,
        DESTRUCT => destruct::destruct(efun_context).await,
        DISABLE_COMMANDS => disable_commands::disable_commands(efun_context).await,
        DUMP => dump::dump(efun_context).await,
        ENABLE_COMMANDS => enable_commands::enable_commands(efun_context).await,
        ENVIRONMENT => environment::environment(efun_context).await,
        EXEC => exec::exec(efun_context).await,
        FILE_NAME => file_name::file_name(efun_context).await,
        FIND_OBJECT => find_object::find_object(efun_context).await,
        INPUT_TO => input_to::input_to(efun_context).await,
        INTERACTIVE => interactive::interactive(efun_context).await,
        LIVING => living::living(efun_context).await,
        MOVE_OBJECT => move_object::move_object(efun_context).await,
        PAPPLYV => papplyv::papplyv(efun_context).await,
        QUERY_CALL_OUT => query_call_out::query_call_out(efun_context).await,
        QUERY_CALL_OUTS => query_call_outs::query_call_outs(efun_context).await,
        QUERY_RESIDENT_MEMORY => query_resident_memory::query_resident_memory(efun_context).await,
        REMOVE_CALL_OUT => remove_call_out::remove_call_out(efun_context).await,
        SET_THIS_PLAYER => set_this_player::set_this_player(efun_context).await,
        TELL_OBJECT => tell_object::tell_object(efun_context).await,
        THIS_OBJECT => this_object::this_object(efun_context).await,
        THIS_PLAYER => this_player::this_player(efun_context).await,
        THROW => throw::throw(efun_context).await,
        WRITE => write::write(efun_context).await,
        WRITE_SOCKET => write_socket::write_socket(efun_context).await,
        _ => Err(efun_context.runtime_error(format!("Unknown efun: {}", efun_name))),
    }
}

/// Global static mapping of all efun names to their prototype.
/// [`Instruction::CallEfun`](lpc_rs_asm::instruction::Instruction::CallEfun) indexes into this map,
/// so changes to the insert order will invalidate previously-compiled code, and break tests.
pub static EFUN_PROTOTYPES: Lazy<IndexMap<&'static str, FunctionPrototype>> = Lazy::new(|| {
    let mut m = IndexMap::new();

    m.insert(
        ALL_ENVIRONMENT,
        FunctionPrototypeBuilder::default()
            .name(ALL_ENVIRONMENT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(true))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 1,
                num_default_args: 1,
                varargs: false,
                ellipsis: false,
            })
            .arg_types(vec![LpcType::String(false) | LpcType::Object(false)])
            .build()
            .expect("failed to build all_environment"),
    );

    m.insert(
        ALL_INVENTORY,
        FunctionPrototypeBuilder::default()
            .name(ALL_INVENTORY)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(true))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 1,
                num_default_args: 1,
                varargs: false,
                ellipsis: false,
            })
            .arg_types(vec![LpcType::String(false) | LpcType::Object(false)])
            .build()
            .expect("failed to build all_inventory"),
    );

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
        DEEP_INVENTORY,
        FunctionPrototypeBuilder::default()
            .name(DEEP_INVENTORY)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(true))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 1,
                num_default_args: 1,
                varargs: false,
                ellipsis: false,
            })
            .arg_types(vec![LpcType::String(false) | LpcType::Object(false)])
            .build()
            .expect("failed to build deep_inventory"),
    );

    m.insert(
        DESTRUCT,
        FunctionPrototypeBuilder::default()
            .name(DESTRUCT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Void)
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Object(false) | LpcType::Object(true)])
            .build()
            .expect("failed to build destruct"),
    );

    m.insert(
        DISABLE_COMMANDS,
        FunctionPrototypeBuilder::default()
            .name(DISABLE_COMMANDS)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Void)
            .kind(FunctionKind::Efun)
            .build()
            .expect("failed to build disable_commands"),
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
        ENABLE_COMMANDS,
        FunctionPrototypeBuilder::default()
            .name(ENABLE_COMMANDS)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Void)
            .kind(FunctionKind::Efun)
            .build()
            .expect("failed to build enable_commands"),
    );

    m.insert(
        ENVIRONMENT,
        FunctionPrototypeBuilder::default()
            .name(ENVIRONMENT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 1,
                num_default_args: 1,
                ellipsis: false,
                varargs: false,
            })
            .arg_types(vec![LpcType::String(false) | LpcType::Object(false)])
            .build()
            .expect("failed to build environment"),
    );

    m.insert(
        EXEC,
        FunctionPrototypeBuilder::default()
            .name(EXEC)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(2))
            .arg_types(vec![LpcType::Object(false), LpcType::Object(false)])
            .build()
            .expect("failed to build exec"),
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
        INPUT_TO,
        FunctionPrototypeBuilder::default()
            .name(INPUT_TO)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Void)
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 2,
                num_default_args: 1,
                varargs: false,
                ellipsis: false,
            })
            .arg_types(vec![LpcType::Function(false), LpcType::Int(false)])
            .build()
            .expect("failed to build input_to"),
    );

    m.insert(
        INTERACTIVE,
        FunctionPrototypeBuilder::default()
            .name(INTERACTIVE)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 1,
                num_default_args: 1,
                ellipsis: false,
                varargs: false,
            })
            .build()
            .expect("failed to build interactive"),
    );

    m.insert(
        LIVING,
        FunctionPrototypeBuilder::default()
            .name(LIVING)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity {
                num_args: 1,
                num_default_args: 1,
                ellipsis: false,
                varargs: false,
            })
            .arg_types(vec![LpcType::String(false) | LpcType::Object(false)])
            .build()
            .expect("failed to build living"),
    );

    m.insert(
        MOVE_OBJECT,
        FunctionPrototypeBuilder::default()
            .name(MOVE_OBJECT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Void)
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::String(false) | LpcType::Object(false)])
            .build()
            .expect("failed to build move_object"),
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
        QUERY_RESIDENT_MEMORY,
        FunctionPrototypeBuilder::default()
            .name(QUERY_RESIDENT_MEMORY)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(0))
            .arg_types(vec![])
            .build()
            .expect("failed to build query_resident_memory"),
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

    m.insert(
        SET_THIS_PLAYER,
        FunctionPrototypeBuilder::default()
            .name(SET_THIS_PLAYER)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Object(false)])
            .build()
            .expect("failed to build set_this_player"),
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
        TELL_OBJECT,
        FunctionPrototypeBuilder::default()
            .name(TELL_OBJECT)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(2))
            .arg_types(vec![
                LpcType::Object(false) | LpcType::String(false),
                LpcType::String(false),
            ])
            .build()
            .expect("failed to build tell_object"),
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
        THIS_PLAYER,
        FunctionPrototypeBuilder::default()
            .name(THIS_PLAYER)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Object(false))
            .kind(FunctionKind::Efun)
            .build()
            .expect("failed to build this_player"),
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

    m.insert(
        WRITE,
        FunctionPrototypeBuilder::default()
            .name(WRITE)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Mixed(false)])
            .build()
            .expect("failed to build write"),
    );

    m.insert(
        WRITE_SOCKET,
        FunctionPrototypeBuilder::default()
            .name(WRITE_SOCKET)
            .filename(LpcPath::InGame("".into()))
            .return_type(LpcType::Int(false))
            .kind(FunctionKind::Efun)
            .arity(FunctionArity::new(1))
            .arg_types(vec![
                LpcType::Int(false) | LpcType::Float(false) | LpcType::String(false),
            ])
            .build()
            .expect("failed to build write_socket"),
    );

    m
});

/// A cache of [`ProgramFunction`]s for all efuns, since they are cloned to each frame.
pub static EFUN_FUNCTIONS: Lazy<IndexMap<&'static str, Arc<ProgramFunction>>> = Lazy::new(|| {
    EFUN_PROTOTYPES
        .iter()
        .map(|(k, v)| {
            let f = ProgramFunction::new(v.clone(), 0);
            (*k, Arc::new(f))
        })
        .collect()
});

/// Helper for efuns with an `ob = this_object()` default argument
fn arg_or_this_object<const N: usize>(
    arg_ref: &LpcRef,
    context: &EfunContext<'_, N>,
) -> Option<Arc<Process>> {
    match arg_ref {
        LpcRef::Int(LpcInt(0)) => Some(context.frame().process.clone()),
        LpcRef::Object(proc) => proc.upgrade(),
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => None,
    }
}
