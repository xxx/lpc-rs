use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::NULL};

/// Enable future idle queries in the calling object, atomically with its writes.
pub fn request_clean_up<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if let Some(cleanup) = &context.process().cleanup {
        context
            .txn()
            .with(|txn| txn.write(cleanup.disabled.id, NULL));
    }
    context.return_efun_result(NULL);
    Ok(())
}
