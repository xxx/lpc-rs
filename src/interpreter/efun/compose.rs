use std::sync::Arc;

use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::{function_arity::FunctionArityBuilder, function_flags::FunctionFlags, lpc_type::LpcType, register::{Register, RegisterVariant}, RegisterSize};
use lpc_rs_errors::Result;
use lpc_rs_function_support::{
    function_prototype::FunctionPrototypeBuilder,
    program_function::{ProgramFunction, ProgramFunctionBuilder},
};
// use logos::Span;
use once_cell::sync::{Lazy, OnceCell};
use string_interner::StringInterner;
use thin_vec::thin_vec;

use crate::interpreter::{
    efun::{efun_context::EfunContext, EFUN_PROTOTYPES},
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
    into_lpc_ref::IntoLpcRef,
    lpc_ref::LpcRef,
};

/// The static composed function handler.
/// It's just a pre-compiled (and slightly optimized) version of:
/// ```c
/// function compose_executor(function f, function g, ...) {
///     return f(papplyv(g, argv)());
/// }
/// ```
pub static COMPOSE_EXECUTOR: Lazy<Arc<ProgramFunction>> = Lazy::new(|| {
    let prototype = FunctionPrototypeBuilder::default()
        .name("compose-executor")
        .filename(Arc::new(Default::default()))
        .return_type(LpcType::Mixed(false))
        .arity(
            FunctionArityBuilder::default()
                .num_args(2)
                .ellipsis(true)
                .build()
                .unwrap(),
        )
        .arg_types(vec![LpcType::Function(false), LpcType::Function(false)])
        .flags(FunctionFlags::default().with_ellipsis(true))
        .build()
        .unwrap();

    let instructions = vec![
        Instruction::PopulateArgv(RegisterVariant::Local(Register(3)), 2, 4),
        Instruction::ClearArgs,
        Instruction::PushArg(RegisterVariant::Local(Register(2))),
        Instruction::PushArg(RegisterVariant::Local(Register(3))),
        Instruction::CallEfun(EFUN_PROTOTYPES.get_index_of("papplyv").unwrap()), // papplyv()
        Instruction::Copy(
            RegisterVariant::Local(Register(0)),
            RegisterVariant::Local(Register(4)),
        ),
        Instruction::ClearArgs,
        Instruction::CallFp(RegisterVariant::Local(Register(4))), // g(argv)
        Instruction::Copy(
            RegisterVariant::Local(Register(0)),
            RegisterVariant::Local(Register(5)),
        ),
        Instruction::ClearArgs,
        Instruction::PushArg(RegisterVariant::Local(Register(5))),
        Instruction::CallFp(RegisterVariant::Local(Register(1))), // f(g(argv))
        Instruction::Ret,
    ];

    let debug_spans = vec![None; instructions.len()];

    let func = ProgramFunctionBuilder::default()
        .prototype(prototype)
        .num_locals(4)
        .num_upvalues(0)
        .instructions(instructions)
        .debug_spans(debug_spans)
        .labels(Default::default())
        .local_variables(vec![])
        .arg_locations(vec![
            RegisterVariant::Local(Register(1)),
            RegisterVariant::Local(Register(2)),
        ])
        .strings(OnceCell::with_value(Arc::new(StringInterner::default())))
        .build()
        .unwrap();

    Arc::new(func)
});

/// `compose`, an efun for composing two functions into a new function.
pub async fn compose<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let a = context.resolve_local_register(1 as RegisterSize).clone();
    if !matches!(a, LpcRef::Function(_)) {
        return Err(context.runtime_error("non-function sent as first argument to `compose`"));
    };

    let b = context.resolve_local_register(2 as RegisterSize).clone();
    if !matches!(b, LpcRef::Function(_)) {
        return Err(context.runtime_error("non-function sent as second argument to `compose`"));
    };

    // This will just create and return a pointer to the `COMPOSE_EXECUTOR` function,
    // which, which called, takes care of actually calling one function, and
    // passing the result to the other.
    let pf = COMPOSE_EXECUTOR.clone();

    let ptr = FunctionPtr {
        owner: Arc::downgrade(&context.frame().process),
        address: FunctionAddress::Local(Arc::downgrade(&context.frame().process), pf),
        partial_args: thin_vec![Some(a), Some(b)],
        call_other: false,
        upvalue_ptrs: thin_vec![],
        unique_id: Default::default(),
    };

    let lpc_ref = ptr.into_lpc_ref(context.memory());

    context.return_efun_result(lpc_ref);

    Ok(())
}
