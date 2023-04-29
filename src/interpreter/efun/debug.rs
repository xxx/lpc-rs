use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

const SNAPSHOT_STACK: &str = "snapshot_stack";

/// `debug`, the kitchen sink efun to do things around getting
/// information from, or debugging the vm itself.
pub async fn debug<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1 as RegisterSize);
    if let LpcRef::String(x) = lpc_ref {
        let r = x.read();
        let str = r.to_str();

        match str {
            SNAPSHOT_STACK => {
                #[cfg(test)]
                {
                    drop(r);
                    snapshot_stack(context)
                }

                #[cfg(not(test))]
                {
                    Err(context.runtime_error("Stack snapshots are only for testing."))
                }
            }
            x => {
                Err(context.runtime_error(format!("Unknown operation `{x}` passed to `debug()`.")))
            }
        }
    } else {
        Err(context.runtime_error(format!(
            "Unexpected argument `{}` passed to `debug()`.",
            lpc_ref
        )))
    }
}

#[cfg(test)]
fn snapshot_stack<const N: usize>(context: &mut EfunContext<N>) -> Result<()> {
    let klone = context.clone_stack();
    context.snapshot = Some(klone);

    Ok(())
}
