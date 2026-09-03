use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::efun::efun_context::EfunContext;

const SNAPSHOT_STACK: &str = "snapshot_stack";

/// `debug`, the kitchen sink efun to do things around getting
/// information from, or debugging the vm itself.
pub fn debug<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1 as RegisterSize);
    let s = lpc_ref.with_string(|x| x.to_string())?;

    match s.as_str() {
        SNAPSHOT_STACK => {
            #[cfg(test)]
            {
                snapshot_stack(context)
            }

            #[cfg(not(test))]
            {
                Err(context.runtime_error("Stack snapshots are only for testing."))
            }
        }
        x => Err(context.runtime_error(format!("Unknown operation `{x}` passed to `debug()`."))),
    }
}

#[cfg(test)]
fn snapshot_stack<const N: usize>(context: &mut EfunContext<N>) -> Result<()> {
    let klone = context.clone_stack();
    context.snapshot = Some(klone);

    Ok(())
}
