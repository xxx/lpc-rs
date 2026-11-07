use std::sync::{Arc, LazyLock};

use lpc_rs_asm::instruction::{Arg, ArgList};
use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType, register::Register};
use lpc_rs_function_support::{
    function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunction,
};
use lpc_rs_utils::config::Config;

use crate::interpreter::{
    call_frame::CallFrame,
    call_stack::CallStack,
    efun::{Efun, efun_context::EfunContext, sprintf, sscanf},
    lpc_ref::LpcRef,
    process::Process,
    program::Program,
    stm::VarId,
    task_context::TaskContext,
    vm::global_state::GlobalState,
};

static STATE: LazyLock<Arc<GlobalState>> = LazyLock::new(|| {
    let (sender, _) = tokio::sync::mpsc::channel(1);
    Arc::new(GlobalState::new(Config::default(), sender))
});

fn formatted(args: Vec<LpcRef>) -> Option<LpcRef> {
    let process = Arc::new(Process::new(Program::default()));
    let task = TaskContext::new(STATE.clone(), process.clone(), None);
    let mut stack = CallStack::<8>::default();
    stack
        .push(CallFrame::entry(process.clone(), task.txn()))
        .unwrap();
    let mut context = EfunContext::fired(&mut stack, &task, Efun::sprintf, args, process, None);
    let result = sprintf::sprintf(&mut context);
    match result {
        Ok(()) => Some(stack.current_frame().unwrap().registers[0].clone()),
        Err(error) => {
            assert!(!error.is_bug(), "{error}");
            None
        }
    }
}

fn scanned(input: &str, format: &str) -> Option<(i64, Vec<LpcRef>)> {
    let process = Arc::new(Process::new(Program::default()));
    let task = TaskContext::new(STATE.clone(), process.clone(), None);
    let cells = std::array::from_fn::<_, 32, _>(|_| VarId::new());
    task.txn().with(|transaction| {
        for &cell in &cells {
            transaction.write(cell, LpcRef::from(0));
        }
    });
    let prototype = FunctionPrototypeBuilder::default()
        .name("fuzz_scan")
        .filename(Arc::new(LpcPath::in_game("/fuzz.c".into())))
        .return_type(LpcType::Int(false))
        .build()
        .unwrap();
    let mut function = ProgramFunction::new(prototype, 2);
    let mut arguments = vec![
        Arg::Value(Register(1).as_local()),
        Arg::Value(Register(2).as_local()),
    ];
    arguments.extend((0..32).map(|index| Arg::Ref(Register(index).as_upvalue())));
    function.arg_lists = vec![arguments];
    let mut frame = CallFrame::new(
        process,
        Arc::new(function),
        0,
        Some(cells.as_slice()),
        task.txn(),
    );
    frame.registers[1] = LpcRef::from(input);
    frame.registers[2] = LpcRef::from(format);
    let mut stack = CallStack::<8>::default();
    stack.push(frame).unwrap();
    let mut context = EfunContext::at_call(&mut stack, &task, Efun::sscanf, ArgList(0)).unwrap();
    let result = sscanf::sscanf(&mut context);
    let values: Vec<_> = task.txn().with(|transaction| {
        cells
            .iter()
            .map(|&cell| transaction.read(cell).unwrap())
            .collect()
    });
    match result {
        Ok(()) => {
            let LpcRef::Int(count) = &stack.current_frame().unwrap().registers[0] else {
                panic!("sscanf must return its conversion count");
            };
            assert!(count.0 >= 0 && count.0 as usize <= format.matches('%').count() + 1);
            Some((count.0, values))
        }
        Err(error) => {
            assert!(!error.is_bug(), "{error}");
            assert!(values.iter().all(|value| *value == LpcRef::from(0)));
            None
        }
    }
}

/// Exercise bounded format rendering, scan cursors, and scalar formatting identities.
pub fn formatting(data: &[u8]) {
    let text = String::from_utf8_lossy(&data[..data.len().min(256)]);
    let (format, input) = text.split_once('\n').unwrap_or((&text, &text));
    let number = data.first().map_or(0, |&byte| i64::from(byte as i8));
    let string = LpcRef::from(input);
    assert_eq!(
        formatted(vec![LpcRef::from("%s"), string.clone()]),
        Some(string.clone())
    );
    assert_eq!(
        formatted(vec![LpcRef::from("%d"), LpcRef::from(number)]),
        Some(LpcRef::from(number.to_string()))
    );
    if sprintf::bounded_format(format) {
        for value in [
            LpcRef::from(number),
            LpcRef::from(number as f64 / 7.0),
            string,
        ] {
            let mut args = vec![LpcRef::from(format)];
            args.extend(std::iter::repeat_n(value, 32));
            let _ = formatted(args);
        }
    }
    sscanf::check_scanners(input, format);
    let _ = scanned(input, format);
    let (count, values) = scanned(&number.to_string(), "%d").unwrap();
    assert_eq!(count, 1);
    assert_eq!(values[0], LpcRef::from(number));
    let (count, values) = scanned(input, "%s").unwrap();
    assert_eq!(count, 1);
    assert_eq!(values[0], LpcRef::from(input));
}
