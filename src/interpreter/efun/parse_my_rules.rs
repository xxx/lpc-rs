//! `parse_my_rules`: `this_object()`'s registered rules, as text.

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `parse_my_rules()`: `this_object()`'s rules, in registration order, each
/// as `"{verb} {rule}"`.
pub async fn parse_my_rules<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let this = context.frame().process.clone();
    let cell = context.object_space().verb_rules.id;
    let rules = context.txn().with(|t| t.read_rules(cell));
    let entries: Vec<LpcRef> = rules
        .iter()
        .filter(|r| r.owned_by(&this))
        .filter_map(|r| {
            r.handler
                .protocol()
                .map(|p| LpcRef::from(format!("{} {}", r.verb, p.rule)))
        })
        .collect();
    context.return_array(entries);
    Ok(())
}
