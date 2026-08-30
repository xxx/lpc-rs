pub mod efun_context;

pub(crate) mod add_action;
pub(crate) mod add_rule;
pub(crate) mod all_environment;
pub(crate) mod all_inventory;
pub(crate) mod call_out;
pub(crate) mod clone_object;
pub(crate) mod command;
pub(crate) mod compose;
pub(crate) mod debug;
pub(crate) mod deep_inventory;
pub(crate) mod destruct;
pub(crate) mod disable_commands;
pub(crate) mod dump;
pub(crate) mod enable_commands;
pub(crate) mod environment;
pub(crate) mod exec;
pub(crate) mod explode;
pub(crate) mod file_name;
pub(crate) mod find_object;
pub(crate) mod implode;
pub(crate) mod input_to;
pub(crate) mod interactive;
pub(crate) mod living;
pub(crate) mod move_object;
pub(crate) mod notify_fail;
pub(crate) mod papplyv;
pub(crate) mod parse_add_rule;
pub(crate) mod parse_add_synonym;
pub(crate) mod parse_command;
pub(crate) mod parse_dump;
pub(crate) mod parse_init;
pub(crate) mod parse_my_rules;
pub(crate) mod parse_refresh;
pub(crate) mod parse_remove;
pub(crate) mod parse_sentence;
pub(crate) mod parse_string;
pub(crate) mod query_call_out;
pub(crate) mod query_call_outs;
pub(crate) mod query_command;
pub(crate) mod query_connection;
pub(crate) mod query_ip_number;
pub(crate) mod query_notify_fail;
pub(crate) mod query_resident_memory;
pub(crate) mod query_verb;
pub(crate) mod remove_action;
pub(crate) mod remove_call_out;
pub(crate) mod remove_rule;
pub(crate) mod send_gmcp;
pub(crate) mod send_mxp;
pub(crate) mod set_this_player;
pub(crate) mod sscanf;
pub(crate) mod tell_object;
pub(crate) mod this_object;
pub(crate) mod this_player;
pub(crate) mod throw;
pub(crate) mod type_predicates;
pub(crate) mod write;
pub(crate) mod write_socket;

use std::sync::Arc;

use indexmap::IndexMap;
use lpc_rs_core::{
    RegisterSize, function_arity::FunctionArity, function_flags::FunctionFlags, lpc_path::LpcPath,
    lpc_type::LpcType,
};
use lpc_rs_errors::Result;
use lpc_rs_function_support::{
    function_prototype::{FunctionKind, FunctionPrototype, FunctionPrototypeBuilder},
    program_function::ProgramFunction,
};
use once_cell::sync::Lazy;
use tracing::trace;

use crate::{
    interpreter::{
        efun::efun_context::EfunContext,
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        process::Process,
        stm::{Effect, TxnHandle},
    },
    telnet::{connection::Connection, ops::ConnectionOp},
};

/// Special forms: typechecked against an efun prototype, compiled to their
/// own instructions.
pub const CALL_OTHER: &str = "call_other";
pub const CATCH: &str = "catch";
pub const SIZEOF: &str = "sizeof";

/// The efun table: one row per efun expands to its dispatch arm and its
/// [`EFUN_PROTOTYPES`] entry.
///
/// Row: `name [option] => { returns: <LpcType>, arity: <arity>, args: [<LpcType>, ..] }`.
/// `arity` is `n` (n arguments), `(n, d)` (the last `d` defaulted) or
/// `(n, d, ellipsis)`, which also sets the prototype's ellipsis flag;
/// `arity` and `args` may be omitted.
/// `[in module]` dispatches to `module::name` instead of `name::name`;
/// `[prototype only]` marks a special form: no module, no dispatch.
/// `refs: from N` marks every argument from index `N` (0-based) as an
/// lvalue the efun writes back through `write_ref`.
macro_rules! efuns {
    (@dispatch $name:ident, $context:ident) => {
        $name::$name($context).await
    };
    (@dispatch $name:ident [in $module:ident], $context:ident) => {
        $module::$name($context).await
    };
    // Unreachable: the compiler never emits `CallEfun` for a special form.
    (@dispatch $name:ident [prototype only], $context:ident) => {
        Err($context.runtime_error(format!("Unknown efun: {}", stringify!($name))))
    };

    (@arity) => {
        FunctionArity::default()
    };
    (@arity $n:literal) => {
        FunctionArity::new($n)
    };
    (@arity ($n:literal, $d:literal $(, ellipsis)?)) => {
        FunctionArity {
            num_args: $n,
            num_default_args: $d,
        }
    };

    (@flags ($n:literal, $d:literal, ellipsis)) => {
        FunctionFlags::default().with_ellipsis(true)
    };
    (@flags $($arity:tt)?) => {
        FunctionFlags::default()
    };

    (@prototype $name:ident {
        returns: $returns:expr
        $(, arity: $arity:tt)?
        $(, args: [$($arg:expr),* $(,)?])?
        $(, refs: from $ref_from:literal)?
        $(,)?
    }) => {
        FunctionPrototypeBuilder::default()
            .name(stringify!($name))
            .filename(LpcPath::InGame("".into()))
            .return_type($returns)
            .kind(FunctionKind::Efun)
            .arity(efuns!(@arity $($arity)?))
            .arg_types(vec![$($($arg),*)?])
            .flags(efuns!(@flags $($arity)?))
            .ref_tail(None $(.or(Some($ref_from)))?)
            .build()
            .expect(concat!("failed to build ", stringify!($name)))
    };

    ($( $name:ident $([$($option:tt)*])? => { $($row:tt)* } ),+ $(,)?) => {
        /// Run the efun named `efun_name` against `efun_context`.
        pub async fn call_efun<const STACKSIZE: usize>(
            efun_name: &str,
            efun_context: &mut EfunContext<'_, STACKSIZE>,
        ) -> Result<()> {
            match efun_name {
                $( stringify!($name) => efuns!(@dispatch $name $([$($option)*])?, efun_context), )+
                _ => Err(efun_context.runtime_error(format!("Unknown efun: {}", efun_name))),
            }
        }

        /// Every efun prototype, in table order.
        /// [`Instruction::CallEfun`](lpc_rs_asm::instruction::Instruction::CallEfun)
        /// indexes into this map; a reorder invalidates compiled code.
        pub static EFUN_PROTOTYPES: Lazy<IndexMap<&'static str, FunctionPrototype>> = Lazy::new(|| {
            let mut m = IndexMap::new();
            $( m.insert(stringify!($name), efuns!(@prototype $name { $($row)* })); )+
            m
        });
    };
}

efuns! {
    add_action => {
        returns: LpcType::Void,
        arity: (3, 1),
        args: [
            LpcType::Function(false) | LpcType::String(false),
            LpcType::String(false) | LpcType::String(true),
            LpcType::Int(false),
        ],
    },
    add_rule => {
        returns: LpcType::Int(false),
        arity: 2,
        args: [
            LpcType::String(false),
            LpcType::Function(false) | LpcType::String(false),
        ],
    },
    all_environment => {
        returns: LpcType::Object(true),
        arity: (1, 1),
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    all_inventory => {
        returns: LpcType::Object(true),
        arity: (1, 1),
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    arrayp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    call_out => {
        returns: LpcType::Int(false),
        arity: (2, 0, ellipsis),
        args: [LpcType::Function(false), LpcType::Int(false) | LpcType::Float(false)],
    },
    call_other [prototype only] => {
        returns: LpcType::Mixed(false),
        arity: (2, 0, ellipsis),
        args: [
            LpcType::Object(false)
                | LpcType::Object(true)
                | LpcType::String(false)
                | LpcType::String(true)
                | LpcType::Mapping(false),
            LpcType::String(false),
        ],
    },
    catch [prototype only] => {
        returns: LpcType::Mixed(false),
        arity: 1,
        args: [LpcType::Mixed(false) | LpcType::Void],
    },
    clone_object => {
        returns: LpcType::Object(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    command => {
        returns: LpcType::Int(false),
        arity: (2, 1),
        args: [LpcType::String(false), LpcType::Object(false)],
    },
    compose => {
        returns: LpcType::Function(false),
        arity: 2,
        args: [LpcType::Function(false), LpcType::Function(false)],
    },
    debug => {
        returns: LpcType::Mixed(false),
        arity: (2, 1),
        args: [LpcType::String(false), LpcType::Mixed(false)],
    },
    deep_inventory => {
        returns: LpcType::Object(true),
        arity: (1, 1),
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    destruct => {
        returns: LpcType::Void,
        arity: 1,
        args: [LpcType::Object(false) | LpcType::Object(true)],
    },
    disable_commands => {
        returns: LpcType::Void,
    },
    dump => {
        returns: LpcType::Void,
        arity: (1, 0, ellipsis),
        args: [LpcType::Mixed(false)],
    },
    enable_commands => {
        returns: LpcType::Void,
    },
    environment => {
        returns: LpcType::Object(false),
        arity: (1, 1),
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    exec => {
        returns: LpcType::Int(false),
        arity: 2,
        args: [LpcType::Object(false), LpcType::Object(false)],
    },
    explode => {
        returns: LpcType::String(true),
        arity: (2, 1),
        args: [LpcType::String(false), LpcType::String(false)],
    },
    file_name => {
        returns: LpcType::String(false),
        arity: 1,
        args: [LpcType::Object(false)],
    },
    find_object => {
        returns: LpcType::Object(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    floatp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    functionp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    implode => {
        returns: LpcType::String(false),
        arity: (2, 1),
        args: [LpcType::String(true), LpcType::String(false)],
    },
    input_to => {
        returns: LpcType::Void,
        arity: (2, 1),
        args: [LpcType::Function(false), LpcType::Int(false)],
    },
    interactive => {
        returns: LpcType::Int(false),
        arity: (1, 1),
    },
    intp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    living => {
        returns: LpcType::Int(false),
        arity: (1, 1),
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    mappingp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    move_object => {
        returns: LpcType::Void,
        arity: 1,
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    notify_fail => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::String(false) | LpcType::Function(false)],
    },
    objectp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    papplyv => {
        returns: LpcType::Function(false),
        arity: (2, 0),
        args: [LpcType::Function(false), LpcType::Mixed(true)],
    },
    parse_add_rule => {
        returns: LpcType::Void,
        arity: 2,
        args: [LpcType::String(false), LpcType::String(false)],
    },
    parse_add_synonym => {
        returns: LpcType::Void,
        arity: (3, 1),
        args: [
            LpcType::String(false),
            LpcType::String(false),
            LpcType::String(false),
        ],
    },
    parse_command => {
        returns: LpcType::Int(false),
        arity: (3, 0, ellipsis),
        args: [
            LpcType::String(false),
            LpcType::Object(false) | LpcType::Object(true),
            LpcType::String(false),
        ],
        refs: from 3,
    },
    parse_dump => {
        returns: LpcType::String(false),
    },
    parse_init => {
        returns: LpcType::Void,
    },
    parse_my_rules => {
        returns: LpcType::String(true),
    },
    parse_refresh => {
        returns: LpcType::Void,
    },
    parse_remove => {
        returns: LpcType::Void,
        arity: 1,
        args: [LpcType::String(false)],
    },
    parse_sentence => {
        returns: LpcType::Mixed(false),
        arity: (4, 3),
        args: [
            LpcType::String(false),
            LpcType::Int(false),
            LpcType::Object(true),
            LpcType::Mapping(false),
        ],
    },
    parse_string => {
        returns: LpcType::Mixed(true),
        arity: (3, 1),
        args: [
            LpcType::String(false),
            LpcType::String(false),
            LpcType::Int(false),
        ],
    },
    query_call_out => {
        returns: LpcType::Mixed(true),
        arity: 1,
        args: [LpcType::Int(false)],
    },
    query_call_outs => {
        returns: LpcType::Mixed(true),
        arity: (1, 1),
    },
    query_command => {
        returns: LpcType::String(false),
    },
    query_notify_fail => {
        returns: LpcType::Mixed(false),
    },
    query_resident_memory => {
        returns: LpcType::Int(false),
        arity: 0,
    },
    query_verb => {
        returns: LpcType::String(false),
        arity: (1, 1),
        args: [LpcType::Int(false)],
    },
    remove_action => {
        returns: LpcType::Int(false),
        arity: (2, 1),
        args: [LpcType::String(false), LpcType::String(false) | LpcType::Object(false)],
    },
    remove_call_out => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Int(false)],
    },
    remove_rule => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Int(false)],
    },
    set_this_player => {
        returns: LpcType::Object(false),
        arity: 1,
        args: [LpcType::Object(false)],
    },
    sizeof [prototype only] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(true) | LpcType::Mapping(false)],
    },
    sscanf => {
        returns: LpcType::Int(false),
        arity: (2, 0, ellipsis),
        args: [LpcType::String(false), LpcType::String(false)],
        refs: from 2,
    },
    stringp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    tell_object => {
        returns: LpcType::Int(false),
        arity: 2,
        args: [LpcType::Object(false) | LpcType::String(false), LpcType::String(false)],
    },
    this_object => {
        returns: LpcType::Object(false),
    },
    this_player => {
        returns: LpcType::Object(false),
    },
    throw => {
        returns: LpcType::Void,
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    write => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    write_socket => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Int(false) | LpcType::Float(false) | LpcType::String(false)],
    },
    query_connection => {
        returns: LpcType::Mapping(false),
        arity: (1, 1),
        args: [LpcType::Object(false)],
    },
    query_ip_number => {
        returns: LpcType::String(false),
        arity: (1, 1),
        args: [LpcType::Object(false)],
    },
    send_gmcp => {
        returns: LpcType::Void,
        arity: 3,
        args: [LpcType::Object(false), LpcType::String(false), LpcType::String(false)],
    },
    send_mxp => {
        returns: LpcType::Void,
        arity: 2,
        args: [LpcType::Object(false), LpcType::String(false)],
    },
}

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

/// The object an optional argument names: `0` is the caller, a string is
/// loaded by path (a failed load is the error), a destructed or non-object
/// argument is `None`.
async fn arg_or_this_object<const N: usize>(
    arg_ref: &LpcRef,
    context: &EfunContext<'_, N>,
) -> Result<Option<Arc<Process>>> {
    Ok(match arg_ref {
        LpcRef::Int(LpcInt(0)) => Some(context.frame().process.clone()),
        LpcRef::String(path) => {
            let path = context.in_game_path(path.to_str());
            Some(context.load_object(&path).await?)
        }
        _ => arg_ref.live_object(context.txn()),
    })
}

/// The connection of the object register 1 names — `this_player()` when the
/// argument is absent or 0 — or `None` when there is none.
fn connection_of<const N: usize>(context: &EfunContext<'_, N>) -> Option<Arc<Connection>> {
    let target = match context.resolve_local_register(1 as RegisterSize) {
        LpcRef::Int(LpcInt(0)) => context.this_player().load_full(),
        arg => arg.live_object(context.txn()),
    };
    target.and_then(|proc| {
        context
            .txn()
            .with(|t| t.read_connection(proc.connection.id))
    })
}

/// Record `op` for the connection of the object register 1 names; nothing but
/// a trace when it has none.
fn send_to_connection<const N: usize>(context: &EfunContext<'_, N>, op: ConnectionOp) {
    let target = context
        .resolve_local_register(1 as RegisterSize)
        .live_object(context.txn());
    let connection = target.and_then(|proc| {
        context
            .txn()
            .with(|t| t.read_connection(proc.connection.id))
    });
    match connection {
        Some(connection) => context.record_effect(Effect::Socket {
            op,
            tx: connection.tx.clone(),
        }),
        None => trace!("{op:?} to an object without a connection"),
    }
}

/// Return `f`'s objects for the object register 1 names (per
/// [`arg_or_this_object`]) as an array of weak references, empty when it
/// names nothing.
async fn return_objects_of<const N: usize, I, F>(
    context: &mut EfunContext<'_, N>,
    f: F,
) -> Result<()>
where
    I: IntoIterator<Item = Arc<Process>>,
    F: FnOnce(&TxnHandle, Arc<Process>) -> I,
{
    let arg_ref = context.resolve_local_register(1 as RegisterSize);
    let objects = arg_or_this_object(arg_ref, context)
        .await?
        .map(|env| f(context.txn(), env));
    let refs: Vec<LpcRef> = objects
        .into_iter()
        .flatten()
        .map(|object| LpcRef::from(Arc::downgrade(&object)))
        .collect();
    context.return_array(refs);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_arity_tuple_sets_the_ellipsis_flag() {
        assert!(EFUN_PROTOTYPES["call_out"].to_string().ends_with(", ...)"));
    }

    /// `CallEfun(u8)` is a position in this list; a reorder breaks every
    /// compiled program.
    #[test]
    fn prototype_order_is_the_abi() {
        let names: Vec<&str> = EFUN_PROTOTYPES.keys().copied().collect();
        assert_eq!(
            names,
            [
                "add_action",
                "add_rule",
                "all_environment",
                "all_inventory",
                "arrayp",
                "call_out",
                "call_other",
                "catch",
                "clone_object",
                "command",
                "compose",
                "debug",
                "deep_inventory",
                "destruct",
                "disable_commands",
                "dump",
                "enable_commands",
                "environment",
                "exec",
                "explode",
                "file_name",
                "find_object",
                "floatp",
                "functionp",
                "implode",
                "input_to",
                "interactive",
                "intp",
                "living",
                "mappingp",
                "move_object",
                "notify_fail",
                "objectp",
                "papplyv",
                "parse_add_rule",
                "parse_add_synonym",
                "parse_command",
                "parse_dump",
                "parse_init",
                "parse_my_rules",
                "parse_refresh",
                "parse_remove",
                "parse_sentence",
                "parse_string",
                "query_call_out",
                "query_call_outs",
                "query_command",
                "query_notify_fail",
                "query_resident_memory",
                "query_verb",
                "remove_action",
                "remove_call_out",
                "remove_rule",
                "set_this_player",
                "sizeof",
                "sscanf",
                "stringp",
                "tell_object",
                "this_object",
                "this_player",
                "throw",
                "write",
                "write_socket",
                "query_connection",
                "query_ip_number",
                "send_gmcp",
                "send_mxp",
            ]
        );
    }
}
