use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
};

/// Enable future idle queries in the calling object, atomically with its writes.
/// Returns LPC `1` if accepted, or `0` if the object is excluded from cleanup.
pub fn request_clean_up<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let accepted = if let Some(cleanup) = &context.process().cleanup
        && context.config().clean_up_interval != 0
        && !context
            .object_space()
            .is_system_key(&context.object_space().process_key(context.process()))
    {
        context
            .txn()
            .with(|txn| txn.write(cleanup.disabled.id, NULL));
        true
    } else {
        false
    };
    context.return_efun_result(LpcRef::from(accepted));
    Ok(())
}
