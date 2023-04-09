use lpc_rs_errors::{LpcError, Result};
use qcell::QCellOwner;

use crate::{
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_value::LpcValue},
    try_extract_value,
    util::keyable::Keyable,
};

const SNAPSHOT_STACK: &str = "snapshot_stack";

/// `debug`, the kitchen sink efun to do things around getting
/// information from, or debugging the vm itself.
pub fn debug<const N: usize>(
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1_usize);
    if let LpcRef::String(x) = lpc_ref {
        let b = x.read();
        let str = try_extract_value!(*b, LpcValue::String).to_str();

        match str {
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
            x => {
                Err(context.runtime_error(format!("Unknown operation `{x}` passed to `debug()`.")))
            }
        }
    } else {
        Err(context.runtime_error(format!(
            "Unexpected argument `{}` passed to `debug()`.",
            lpc_ref.with_key(cell_key)
        )))
    }
}

#[cfg(test)]
fn snapshot_stack<const N: usize>(context: &mut EfunContext<N>) -> Result<()> {
    let klone = context.clone_stack();
    context.snapshot = Some(klone);

    Ok(())
}
