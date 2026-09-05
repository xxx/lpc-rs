pub mod efun_context;

pub(crate) mod abs;
pub(crate) mod add_action;
pub(crate) mod add_rule;
pub(crate) mod all_environment;
pub(crate) mod all_inventory;
pub(crate) mod allocate;
pub(crate) mod bits;
pub(crate) mod call_out;
pub(crate) mod callback;
pub(crate) mod case;
pub(crate) mod clone_object;
pub(crate) mod command;
pub(crate) mod compose;
pub(crate) mod conversions;
pub(crate) mod crypt;
pub(crate) mod ctime;
pub(crate) mod debug;
pub(crate) mod deep_inventory;
pub(crate) mod destruct;
pub(crate) mod disable_commands;
pub(crate) mod dump;
pub(crate) mod enable_commands;
pub(crate) mod environment;
pub(crate) mod exec;
pub(crate) mod explode;
pub(crate) mod file_access;
pub(crate) mod file_name;
pub(crate) mod file_size;
pub(crate) mod filter;
pub(crate) mod filter_map;
pub(crate) mod find_object;
pub(crate) mod function_exists;
pub(crate) mod get_dir;
pub(crate) mod implode;
pub(crate) mod input_to;
pub(crate) mod interactive;
pub(crate) mod json_decode;
pub(crate) mod json_encode;
pub(crate) mod keys_values;
pub(crate) mod living;
pub(crate) mod localtime;
pub(crate) mod m_delete;
pub(crate) mod map;
pub(crate) mod math;
pub(crate) mod member_array;
pub(crate) mod min_max;
pub(crate) mod mkdir;
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
pub(crate) mod present;
pub(crate) mod previous_object;
pub(crate) mod query_call_out;
pub(crate) mod query_call_outs;
pub(crate) mod query_command;
pub(crate) mod query_connection;
pub(crate) mod query_ip_number;
pub(crate) mod query_notify_fail;
pub(crate) mod query_resident_memory;
pub(crate) mod query_verb;
pub(crate) mod random;
pub(crate) mod read_file;
pub(crate) mod regexp;
pub(crate) mod remove_action;
pub(crate) mod remove_call_out;
pub(crate) mod remove_rule;
pub(crate) mod rename;
pub(crate) mod rm;
pub(crate) mod rmdir;
pub(crate) mod send_gmcp;
pub(crate) mod send_mxp;
pub(crate) mod set_this_player;
pub(crate) mod sort_array;
pub(crate) mod sprintf;
pub(crate) mod sscanf;
pub(crate) mod tell_object;
pub(crate) mod this_interactive;
pub(crate) mod this_object;
pub(crate) mod this_player;
pub(crate) mod throw;
pub(crate) mod time;
pub(crate) mod type_of;
pub(crate) mod type_predicates;
pub(crate) mod unique_array;
pub(crate) mod users;
pub(crate) mod write;
pub(crate) mod write_file;
pub(crate) mod write_socket;

use std::sync::Arc;

use indexmap::IndexMap;
use lpc_rs_core::{
    function_arity::FunctionArity, function_flags::FunctionFlags, lpc_path::LpcPath,
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

/// The name an efun row spells, without the `r#` a Rust keyword needs
/// (`r#typeof`).
fn efun_name(spelled: &'static str) -> &'static str {
    spelled.strip_prefix("r#").unwrap_or(spelled)
}

/// The efun table: one row per efun expands to its dispatch arm and its
/// [`EFUN_PROTOTYPES`] entry.
///
/// Row: `name [option] => { returns: <LpcType>, arity: <arity>, args: [<LpcType>, ..] }`.
/// A name that is a Rust keyword is spelled raw (`r#typeof`).
/// `arity` is `n` (n arguments), `(n, d)` (the last `d` defaulted) or
/// `(n, d, ellipsis)`, which also sets the prototype's ellipsis flag;
/// `arity` and `args` may be omitted.
/// `[in module]` dispatches to `module::name` instead of `name::name`;
/// `[async]` (or `[async in module]`) marks an efun that can suspend; an
/// unmarked one is a plain `fn` run inside
/// [`Task::step`](crate::interpreter::task::Task::step), no future built;
/// `[prototype only]` marks a special form: no module, no dispatch.
/// `refs: from N` marks every argument from index `N` (0-based) as an
/// lvalue the efun writes back through `write_ref`.
macro_rules! efuns {
    (@dispatch $name:ident, $context:ident) => {
        $name::$name($context)
    };
    (@dispatch $name:ident [in $module:ident], $context:ident) => {
        $module::$name($context)
    };
    (@dispatch $name:ident [async], $context:ident) => {
        $name::$name($context).await
    };
    (@dispatch $name:ident [async in $module:ident], $context:ident) => {
        $module::$name($context).await
    };
    // Unreachable: the compiler never emits `CallEfun` for a special form.
    (@dispatch $name:ident [prototype only], $context:ident) => {
        Err($context.runtime_error(format!("Unknown efun: {}", stringify!($name))))
    };

    (@sync $name:ident [async $($rest:tt)*], $context:ident) => {
        None
    };
    (@sync $name:ident $([$($option:tt)*])?, $context:ident) => {
        Some(efuns!(@dispatch $name $([$($option)*])?, $context))
    };

    (@suspends [async $($rest:tt)*]) => {
        true
    };
    (@suspends $([$($option:tt)*])?) => {
        false
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
            .name(efun_name(stringify!($name)))
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
        /// Every efun; the discriminant is its [`EFUN_PROTOTYPES`] index,
        /// the one a [`CallEfun`](lpc_rs_asm::instruction::Instruction::CallEfun)
        /// carries.
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[expect(non_camel_case_types, reason = "one variant per efun, spelled as its table row is")]
        pub enum Efun {
            $( $name, )+
        }

        impl Efun {
            const ALL: &'static [Efun] = &[$( Efun::$name, )+];
            const SUSPENDS: &'static [bool] = &[$( efuns!(@suspends $([$($option)*])?), )+];
        }

        /// Run `efun` against `efun_context`.
        pub async fn call_efun<const STACKSIZE: usize>(
            efun: Efun,
            efun_context: &mut EfunContext<'_, STACKSIZE>,
        ) -> Result<()> {
            match efun {
                $( Efun::$name => efuns!(@dispatch $name $([$($option)*])?, efun_context), )+
            }
        }

        /// Run `efun` when it never suspends; `None` for one that awaits,
        /// which only [`call_efun`] runs.
        pub fn call_efun_sync<const STACKSIZE: usize>(
            efun: Efun,
            efun_context: &mut EfunContext<'_, STACKSIZE>,
        ) -> Option<Result<()>> {
            match efun {
                $( Efun::$name => efuns!(@sync $name $([$($option)*])?, efun_context), )+
            }
        }

        /// Every efun prototype, in table order.
        /// [`Instruction::CallEfun`](lpc_rs_asm::instruction::Instruction::CallEfun)
        /// indexes into this map; a reorder invalidates compiled code.
        pub static EFUN_PROTOTYPES: Lazy<IndexMap<&'static str, FunctionPrototype>> = Lazy::new(|| {
            let mut m = IndexMap::new();
            $( m.insert(efun_name(stringify!($name)), efuns!(@prototype $name { $($row)* })); )+
            m
        });
    };
}

/// What `min` and `max` take: numbers, or one array of them.
/// An int or a float.
fn number() -> LpcType {
    LpcType::Int(false) | LpcType::Float(false)
}

fn numbers() -> LpcType {
    LpcType::Int(false)
        | LpcType::Float(false)
        | LpcType::Int(true)
        | LpcType::Float(true)
        | LpcType::Mixed(true)
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
    all_environment [async] => {
        returns: LpcType::Object(true),
        arity: (1, 1),
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    all_inventory [async] => {
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
    clone_object [async] => {
        returns: LpcType::Object(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    command [async] => {
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
    deep_inventory [async] => {
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
    dump [async] => {
        returns: LpcType::Void,
        arity: (1, 0, ellipsis),
        args: [LpcType::Mixed(false)],
    },
    enable_commands => {
        returns: LpcType::Void,
    },
    environment [async] => {
        returns: LpcType::Object(false),
        arity: (1, 1),
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    exec [async] => {
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
    find_object [async] => {
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
    interactive [async] => {
        returns: LpcType::Int(false),
        arity: (1, 1),
    },
    intp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    living [async] => {
        returns: LpcType::Int(false),
        arity: (1, 1),
        args: [LpcType::String(false) | LpcType::Object(false)],
    },
    mappingp [in type_predicates] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    move_object [async] => {
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
    parse_command [async] => {
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
    parse_sentence [async] => {
        returns: LpcType::Mixed(false),
        arity: (4, 3),
        args: [
            LpcType::String(false),
            LpcType::Int(false),
            LpcType::Object(true),
            LpcType::Mapping(false),
        ],
    },
    parse_string [async] => {
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
    tell_object [async] => {
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
    write [async] => {
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
    get_dir [async] => {
        returns: LpcType::String(true),
        arity: 1,
        args: [LpcType::String(false)],
    },
    read_file [async] => {
        returns: LpcType::String(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    rm [async] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    write_file [async] => {
        returns: LpcType::Int(false),
        arity: 2,
        args: [LpcType::String(false), LpcType::String(false)],
    },
    json_encode => {
        returns: LpcType::String(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    json_decode => {
        returns: LpcType::Mixed(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    previous_object => {
        returns: LpcType::Mixed(false),
        arity: (1, 1),
        args: [LpcType::Int(false)],
    },
    abs => {
        returns: LpcType::Mixed(false),
        arity: 1,
        args: [LpcType::Int(false) | LpcType::Float(false)],
    },
    capitalize [in case] => {
        returns: LpcType::String(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    ctime => {
        returns: LpcType::String(false),
        arity: (1, 1),
        args: [LpcType::Int(false)],
    },
    lower_case [in case] => {
        returns: LpcType::String(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    max [in min_max] => {
        returns: LpcType::Mixed(false),
        arity: (1, 0, ellipsis),
        args: [numbers()],
    },
    min [in min_max] => {
        returns: LpcType::Mixed(false),
        arity: (1, 0, ellipsis),
        args: [numbers()],
    },
    random => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Int(false)],
    },
    time => {
        returns: LpcType::Int(false),
    },
    to_float [in conversions] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    to_int [in conversions] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    to_string [in conversions] => {
        returns: LpcType::String(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    r#typeof [in type_of] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::Mixed(false)],
    },
    upper_case [in case] => {
        returns: LpcType::String(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    allocate => {
        returns: LpcType::Mixed(true),
        arity: (2, 1),
        args: [LpcType::Int(false), LpcType::Mixed(false)],
    },
    keys [in keys_values] => {
        returns: LpcType::Mixed(true),
        arity: 1,
        args: [LpcType::Mapping(false)],
    },
    m_delete => {
        returns: LpcType::Mapping(false),
        arity: 2,
        args: [LpcType::Mapping(false), LpcType::Mixed(false)],
    },
    member_array => {
        returns: LpcType::Int(false),
        arity: (3, 1),
        args: [LpcType::Mixed(false), LpcType::Mixed(true), LpcType::Int(false)],
    },
    values [in keys_values] => {
        returns: LpcType::Mixed(true),
        arity: 1,
        args: [LpcType::Mapping(false)],
    },
    function_exists [async] => {
        returns: LpcType::Mixed(false),
        arity: (2, 1),
        args: [LpcType::String(false), LpcType::String(false) | LpcType::Object(false)],
    },
    this_interactive => {
        returns: LpcType::Object(false),
    },
    users => {
        returns: LpcType::Object(true),
    },
    file_size [async] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    mkdir [async] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    rename [async] => {
        returns: LpcType::Int(false),
        arity: 2,
        args: [LpcType::String(false), LpcType::String(false)],
    },
    rmdir [async] => {
        returns: LpcType::Int(false),
        arity: 1,
        args: [LpcType::String(false)],
    },
    filter => {
        returns: LpcType::Mixed(false),
        arity: (2, 0, ellipsis),
        args: [LpcType::Mixed(true) | LpcType::Mapping(false), LpcType::Function(false)],
    },
    map => {
        returns: LpcType::Mixed(false),
        arity: (2, 0, ellipsis),
        args: [LpcType::Mixed(true) | LpcType::Mapping(false), LpcType::Function(false)],
    },
    sort_array => {
        returns: LpcType::Mixed(true),
        arity: 2,
        args: [LpcType::Mixed(true), LpcType::Function(false) | LpcType::Int(false)],
    },
    present => {
        returns: LpcType::Object(false),
        arity: (2, 1),
        args: [LpcType::String(false) | LpcType::Object(false), LpcType::Object(false)],
    },
    sprintf => {
        returns: LpcType::String(false),
        arity: (1, 0, ellipsis),
        args: [LpcType::String(false)],
    },
    filter_map => {
        returns: LpcType::Mixed(false),
        arity: (2, 0, ellipsis),
        args: [LpcType::Mixed(true) | LpcType::Mapping(false), LpcType::Function(false)],
    },
    crypt => {
        returns: LpcType::String(false),
        arity: (2, 1),
        args: [LpcType::String(false), LpcType::String(false) | LpcType::Int(false)],
    },
    sin [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    cos [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    tan [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    asin [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    acos [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    atan [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    atan2 [in math] => {
        returns: LpcType::Float(false),
        arity: 2,
        args: [number(), number()],
    },
    exp [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    log [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    pow [in math] => {
        returns: LpcType::Float(false),
        arity: 2,
        args: [number(), number()],
    },
    sqrt [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    floor [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    ceil [in math] => {
        returns: LpcType::Float(false),
        arity: 1,
        args: [number()],
    },
    set_bit [in bits] => {
        returns: LpcType::String(false),
        arity: 2,
        args: [LpcType::String(false), LpcType::Int(false)],
    },
    clear_bit [in bits] => {
        returns: LpcType::String(false),
        arity: 2,
        args: [LpcType::String(false), LpcType::Int(false)],
    },
    test_bit [in bits] => {
        returns: LpcType::Int(false),
        arity: 2,
        args: [LpcType::String(false), LpcType::Int(false)],
    },
    localtime => {
        returns: LpcType::Int(true),
        arity: (1, 1),
        args: [LpcType::Int(false)],
    },
    strftime [in localtime] => {
        returns: LpcType::String(false),
        arity: (2, 1),
        args: [LpcType::String(false), LpcType::Int(false)],
    },
    regexp => {
        returns: LpcType::Mixed(true),
        arity: (3, 1),
        args: [LpcType::Mixed(true), LpcType::String(false), LpcType::Int(false)],
    },
    unique_array => {
        returns: LpcType::Mixed(true),
        arity: (3, 1),
        args: [LpcType::Mixed(true), LpcType::Function(false), LpcType::Mixed(false)],
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

impl Efun {
    /// The efun at a
    /// [`CallEfun`](lpc_rs_asm::instruction::Instruction::CallEfun) index,
    /// `None` past the table.
    #[inline]
    pub fn from_index(index: usize) -> Option<Efun> {
        Self::ALL.get(index).copied()
    }

    /// The efun with this name, `None` for a name outside the table.
    pub fn from_name(name: &str) -> Option<Efun> {
        EFUN_PROTOTYPES
            .get_index_of(name)
            .and_then(Self::from_index)
    }

    /// Whether this efun can suspend (an `[async]` row).
    #[inline]
    pub fn suspends(self) -> bool {
        Self::SUSPENDS[self as usize]
    }

    /// The efun's prototype.
    #[inline]
    pub fn prototype(self) -> &'static FunctionPrototype {
        &EFUN_PROTOTYPES[self as usize]
    }

    /// The efun's name.
    #[inline]
    pub fn name(self) -> &'static str {
        EFUN_PROTOTYPES
            .get_index(self as usize)
            .map_or("", |(name, _)| name)
    }
}

/// The object an optional argument names: `0` is the caller, a string is
/// loaded by path (a failed load is the error), a destructed or non-object
/// argument is `None`.
async fn arg_or_this_object<const N: usize>(
    arg_ref: &LpcRef,
    context: &EfunContext<'_, N>,
) -> Result<Option<Arc<Process>>> {
    Ok(match arg_ref {
        LpcRef::Int(LpcInt(0)) => Some(context.process().clone()),
        LpcRef::String(path) => Some(context.load_object(path.to_str()).await?),
        _ => arg_ref.live_object(context.txn()),
    })
}

/// `process`'s file as an in-game path without its extension
/// (`/secure/master`), what `file_name` and `to_string` answer.
pub(crate) fn in_game_name<const N: usize>(
    context: &EfunContext<'_, N>,
    process: &Process,
) -> String {
    LpcPath::new_server(&*process.filename())
        .as_in_game(&*context.config().lib_dir)
        .to_string_lossy()
        .into_owned()
}

/// The connection of the object register 1 names — `this_player()` when the
/// argument is absent or 0 — or `None` when there is none.
fn connection_of<const N: usize>(context: &EfunContext<'_, N>) -> Option<Arc<Connection>> {
    let target = match context.arg(0) {
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
    let target = context.arg(0).live_object(context.txn());
    let connection = target.and_then(|proc| {
        context
            .txn()
            .with(|t| t.read_connection(proc.connection.id))
    });
    match connection {
        Some(connection) => context.record_effect(Effect::Socket {
            op,
            tx: connection.sender(),
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
    let arg_ref = context.arg(0);
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

    /// `[async]` rows suspend; the rest never do.
    #[test]
    fn suspends_follows_the_rows() {
        assert!(Efun::clone_object.suspends());
        assert!(!Efun::this_object.suspends());
        assert!(!Efun::intp.suspends());
    }

    /// `Efun`'s discriminants are the table positions `CallEfun` carries.
    #[test]
    fn efun_ids_follow_the_prototype_table() {
        for (index, name) in EFUN_PROTOTYPES.keys().enumerate() {
            let efun = Efun::from_index(index).expect(name);
            assert_eq!(
                (efun as usize, format!("{efun:?}").as_str()),
                (index, *name)
            );
            assert_eq!(Efun::from_name(name), Some(efun));
        }
        assert_eq!(Efun::from_index(EFUN_PROTOTYPES.len()), None);
    }

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
                "get_dir",
                "read_file",
                "rm",
                "write_file",
                "json_encode",
                "json_decode",
                "previous_object",
                "abs",
                "capitalize",
                "ctime",
                "lower_case",
                "max",
                "min",
                "random",
                "time",
                "to_float",
                "to_int",
                "to_string",
                "typeof",
                "upper_case",
                "allocate",
                "keys",
                "m_delete",
                "member_array",
                "values",
                "function_exists",
                "this_interactive",
                "users",
                "file_size",
                "mkdir",
                "rename",
                "rmdir",
                "filter",
                "map",
                "sort_array",
                "present",
                "sprintf",
                "filter_map",
                "crypt",
                "sin",
                "cos",
                "tan",
                "asin",
                "acos",
                "atan",
                "atan2",
                "exp",
                "log",
                "pow",
                "sqrt",
                "floor",
                "ceil",
                "set_bit",
                "clear_bit",
                "test_bit",
                "localtime",
                "strftime",
                "regexp",
                "unique_array",
            ]
        );
    }
}
