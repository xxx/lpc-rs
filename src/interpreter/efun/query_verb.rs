use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_string::LpcString};

/// `query_verb`, an efun returning the verb of the command in progress; with a
/// nonzero argument, the first word as typed. 0 outside a command.
pub async fn query_verb<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let full = matches!(
        context.try_resolve_local_register(1 as RegisterSize),
        Some(LpcRef::Int(flag)) if flag.0 != 0
    );
    let verb = context.task_context().with_command(|state| {
        state.map(|state| {
            if full {
                state.verb_typed.clone()
            } else {
                state.verb_reported.clone()
            }
        })
    });
    if let Some(verb) = verb {
        context.return_efun_result(LpcString::from(verb.as_str()).into());
    }
    Ok(())
}
