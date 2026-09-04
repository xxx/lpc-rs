//! `parse_my_rules`: `this_object()`'s registered rules, as text.

use lpc_rs_errors::Result;

use crate::{
    command::registry::VerbRules,
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `parse_my_rules()`: `this_object()`'s rules, in registration order, each
/// as `"{verb} {rule}"`.
pub fn parse_my_rules<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let this = context.process().clone();
    let entries: Vec<LpcRef> = VerbRules::new(context.task_context())
        .owned_by(&this)
        .iter()
        .filter_map(|r| {
            r.protocol()
                .map(|p| LpcRef::from(format!("{} {}", r.verb, p.rule)))
        })
        .collect();
    context.return_array(entries);
    Ok(())
}
