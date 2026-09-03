//! `parse_init`: mark `this_object()` a verb object.

use lpc_rs_errors::Result;

use crate::interpreter::efun::efun_context::EfunContext;

/// `parse_init()`: `this_object()` may now call `parse_add_rule`.
pub fn parse_init<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let this = context.frame().process.clone();
    if !this.is_live(context.txn()) {
        return Err(context.runtime_error("parse_init: this_object() is not live"));
    }
    let _ = this.parser_ready.set(());
    Ok(())
}
