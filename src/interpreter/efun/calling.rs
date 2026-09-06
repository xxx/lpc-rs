//! `calling_function` and `calling_program`: the function that called
//! through the door `previous_object` names, and its defining file.

use std::sync::Arc;

use lpc_rs_errors::Result;
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
};

/// `calling_function([step])`: the name of the function that called
/// through the door `previous_object(step)` names; 0 where the driver
/// fired the call. `-1` is the whole chain as an array.
pub fn calling_function<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    answer(context, "calling_function", |function| {
        LpcRef::from(function.name().as_ref())
    })
}

/// `calling_program([step])`: the defining file of the function
/// `calling_function(step)` names, as an in-game path with its extension
/// (`/secure/master.c`); 0 where the driver fired the call.
pub fn calling_program<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lib_dir = context.config().lib_dir;
    answer(context, "calling_program", |function| {
        let path = function.prototype.filename.as_in_game(lib_dir.as_str());
        LpcRef::from(path.display().to_string())
    })
}

/// What `name` makes of the caller `step` back — 0 for one with no
/// function — or of every caller for `-1`; another negative step is an
/// error naming `efun`.
fn answer<const N: usize>(
    context: &mut EfunContext<'_, N>,
    efun: &str,
    name: impl Fn(&ProgramFunction) -> LpcRef,
) -> Result<()> {
    let step = match context.try_arg(0) {
        Some(LpcRef::Int(n)) => n.0,
        _ => 0,
    };
    let of = |function: Option<&Arc<ProgramFunction>>| function.map_or(NULL, |f| name(f));
    let result = match step {
        -1 => context.mint_array(context.callers().map(|(_, function)| of(function))),
        n if n >= 0 => usize::try_from(n)
            .ok()
            .and_then(|n| context.callers().nth(n))
            .map_or(NULL, |(_, function)| of(function)),
        n => {
            return Err(
                context.runtime_error(format!("{efun}: expected a step back or -1, got {n}"))
            );
        }
    };
    context.return_efun_result(result);
    Ok(())
}
