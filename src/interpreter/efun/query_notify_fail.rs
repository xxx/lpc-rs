use lpc_rs_errors::Result;

use crate::interpreter::efun::efun_context::EfunContext;

/// `query_notify_fail`, an efun returning the pending `notify_fail` message
/// of the command in progress; 0 when none.
pub async fn query_notify_fail<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let pending = context
        .task_context()
        .with_command(|state| state.and_then(|state| state.notify_fail.clone()));
    if let Some(pending) = pending {
        context.return_efun_result(pending);
    }
    Ok(())
}
