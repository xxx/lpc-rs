use std::rc::Rc;
use std::sync::Arc;
// use logos::Span;
use once_cell::sync::{Lazy, OnceCell};
use lpc_rs_errors::Result;
use qcell::QCellOwner;
use string_interner::{StringInterner};
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::function_arity::{FunctionArityBuilder};
use lpc_rs_core::function_flags::FunctionFlags;
use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_core::register::{Register, RegisterVariant};

use lpc_rs_function_support::function_prototype::{FunctionPrototypeBuilder};
use lpc_rs_function_support::program_function::{ProgramFunction, ProgramFunctionBuilder};

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_ref::LpcRef,
    lpc_value::LpcValue,
};
use crate::interpreter::efun::EFUN_PROTOTYPES;
use crate::interpreter::function_type::function_address::FunctionAddress;
use crate::interpreter::function_type::function_ptr::FunctionPtr;

/// The static composed function handler.
/// It's just a pre-compiled (and slightly optimized) version of:
/// ```lpc
/// function compose_executor(function f, function g, ...) {
///     return f(papplyv(g, argv)());
/// }
/// ```
pub static COMPOSE_EXECUTOR: Lazy<Arc<ProgramFunction>> = Lazy::new(|| {
    let prototype = FunctionPrototypeBuilder::default()
        .name("compose-executor")
        .filename(Arc::new(Default::default()))
        .return_type(LpcType::Mixed(false))
        .arity(FunctionArityBuilder::default().num_args(2).ellipsis(true).build().unwrap())
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
        Instruction::Copy(RegisterVariant::Local(Register(0)), RegisterVariant::Local(Register(4))),
        Instruction::ClearArgs,
        Instruction::CallFp(RegisterVariant::Local(Register(4))), // g(argv)
        Instruction::Copy(RegisterVariant::Local(Register(0)), RegisterVariant::Local(Register(5))),
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
        .arg_locations(vec![RegisterVariant::Local(Register(1)), RegisterVariant::Local(Register(2))])
        .strings(OnceCell::with_value(Arc::new(StringInterner::default())))
        .build()
        .unwrap();

    Arc::new(func)
});

/// `compose`, an efun for composing two functions into a new function.
pub fn compose<const N: usize>(
    context: &mut EfunContext<N>,
    _cell_key: &mut QCellOwner,
) -> Result<()> {
    let a = context.resolve_local_register(1_usize);
    if !matches!(a, LpcRef::Function(_)) {
        return Err(context.runtime_error("non-function sent as first argument to `compose`"));
    };

    let b = context.resolve_local_register(2_usize);
    if !matches!(b, LpcRef::Function(_)) {
        return Err(context.runtime_error("non-function sent as second argument to `compose`"));
    };

    let pf = COMPOSE_EXECUTOR.clone();

    let ptr = FunctionPtr {
        owner: Rc::downgrade(&context.frame().process),
        address: FunctionAddress::Local(context.frame().process.clone(), pf),
        partial_args: vec![Some(a), Some(b)],
        call_other: false,
        upvalue_ptrs: vec![],
        unique_id: Default::default(),
    };

    let value = LpcValue::Function(ptr);
    let lpc_ref = context.value_to_ref(value);

    context.return_efun_result(lpc_ref);

    Ok(())
}