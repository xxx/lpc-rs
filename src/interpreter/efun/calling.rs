//! The object, function, and defining file of each calling frame.

use std::sync::Arc;

use lpc_rs_core::lpc_path::LibRoot;
use lpc_rs_errors::Result;
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
    process::Process,
};

/// The object of the calling frame, including local calls; `-1` returns all callers.
pub fn calling_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let txn = context.txn().clone();
    answer(context, "calling_object", |process, _| {
        if process.is_live(&txn) {
            LpcRef::from(Arc::downgrade(process))
        } else {
            NULL
        }
    })
}

/// The calling function's name, including local calls; 0 for a driver frame.
pub fn calling_function<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    answer(context, "calling_function", |_, function| {
        function.map_or(NULL, |function| LpcRef::from(function.name().as_ref()))
    })
}

/// `calling_program([step])`: the defining file of the function
/// `calling_function(step)` names, as an in-game path with its extension
/// (`/secure/master.c`); 0 where the driver fired the call.
pub fn calling_program<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lib_dir = context.config().lib_dir;
    answer(context, "calling_program", |_, function| {
        function.map_or(NULL, |function| {
            let path = LibRoot::new(lib_dir.as_str()).source_name(&function.prototype.filename);
            LpcRef::from(path.to_string())
        })
    })
}

fn answer<const N: usize>(
    context: &mut EfunContext<'_, N>,
    efun: &str,
    name: impl Fn(&Arc<Process>, Option<&Arc<ProgramFunction>>) -> LpcRef,
) -> Result<()> {
    let step = match context.try_arg(0) {
        Some(LpcRef::Int(n)) => n.0,
        _ => 0,
    };
    let result = match step {
        -1 => context.mint_array(
            context
                .calling_frames()
                .map(|(object, function)| name(object, function)),
        ),
        n if n >= 0 => usize::try_from(n)
            .ok()
            .and_then(|n| context.calling_frames().nth(n))
            .map_or(NULL, |(object, function)| name(object, function)),
        n => {
            return Err(
                context.runtime_error(format!("{efun}: expected a step back or -1, got {n}"))
            );
        }
    };
    context.return_efun_result(result);
    Ok(())
}
