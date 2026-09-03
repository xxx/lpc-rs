use std::sync::Arc;

use lpc_rs_asm::instruction::{Arg, ArgList, Instruction};
use lpc_rs_core::{
    RegisterSize,
    function_arity::FunctionArity,
    function_flags::FunctionFlags,
    lpc_type::LpcType,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::Result;
use lpc_rs_function_support::{
    function_prototype::FunctionPrototypeBuilder,
    program_function::{ProgramFunction, ProgramFunctionBuilder},
};
// use logos::Span;
use once_cell::sync::Lazy;
use thin_vec::thin_vec;

use crate::interpreter::{
    efun::{EFUN_PROTOTYPES, efun_context::EfunContext},
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
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
        .arity(FunctionArity::new(2))
        .arg_types(vec![LpcType::Function(false), LpcType::Function(false)])
        .flags(FunctionFlags::default().with_ellipsis(true))
        .build()
        .unwrap();

    let instructions = vec![
        Instruction::PopulateArgv(RegisterVariant::Local(Register(3)), 2, 4),
        Instruction::CallEfun(
            u8::try_from(EFUN_PROTOTYPES.get_index_of("papplyv").unwrap()).unwrap(),
            ArgList(0),
        ), // papplyv(g, argv)
        Instruction::Copy(
            RegisterVariant::Local(Register(0)),
            RegisterVariant::Local(Register(4)),
        ),
        Instruction::CallFp(RegisterVariant::Local(Register(4)), ArgList(1)), // g(argv)
        Instruction::Copy(
            RegisterVariant::Local(Register(0)),
            RegisterVariant::Local(Register(5)),
        ),
        Instruction::CallFp(RegisterVariant::Local(Register(1)), ArgList(2)), // f(g(argv))
        Instruction::Ret,
    ];

    let arg_lists = vec![
        vec![
            Arg::Value(RegisterVariant::Local(Register(2))),
            Arg::Value(RegisterVariant::Local(Register(3))),
        ],
        vec![],
        vec![Arg::Value(RegisterVariant::Local(Register(5)))],
    ];
    let debug_spans = vec![None; instructions.len()];

    let func = ProgramFunctionBuilder::default()
        .prototype(prototype)
        .num_locals(4)
        .num_upvalues(0)
        .arg_lists(arg_lists)
        .instructions(instructions)
        .debug_spans(debug_spans)
        .labels(Default::default())
        .local_variables(vec![])
        .arg_locations(vec![
            RegisterVariant::Local(Register(1)),
            RegisterVariant::Local(Register(2)),
        ])
        .build()
        .unwrap();

    Arc::new(func)
});

/// `compose`, an efun for composing two functions into a new function.
pub fn compose<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
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
    let executor = COMPOSE_EXECUTOR.clone();

    let ptr = FunctionPtrBuilder::default()
        .owner(Arc::downgrade(&context.frame().process))
        .address(FunctionAddress::Local(
            Arc::downgrade(&context.frame().process),
            executor,
        ))
        .partial_args(thin_vec![Some(a), Some(b)])
        .build()
        .unwrap();

    let lpc_ref = ptr.into();

    context.return_efun_result(lpc_ref);

    Ok(())
}
