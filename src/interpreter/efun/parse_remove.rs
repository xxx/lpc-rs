//! `parse_remove`: drop `this_object()`'s rules for a verb.

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, stm::MergeOp};

/// `parse_remove(verb)`: drops every rule `this_object()` registered whose
/// base verb (`handler.protocol().verb`) is `verb`, synonyms included, and
/// purges any rule left behind by a destructed owner.
pub async fn parse_remove<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::String(verb) = context.resolve_local_register(1 as RegisterSize).clone() else {
        return Err(context.runtime_error("parse_remove: the verb must be a string"));
    };
    let this = context.frame().process.clone();
    let verb = verb.to_str();
    let cell = context.object_space().verb_rules.id;
    context.txn().with(|t| {
        let rules = t.read_rules(cell);
        for rule in rules.iter() {
            if rule.owner().is_none() {
                t.merge(cell, MergeOp::RulesRemove(rule.id));
                continue;
            }
            let is_owned = rule.owned_by(&this);
            let is_base_verb = rule
                .handler
                .protocol()
                .is_some_and(|p| p.verb.as_str() == verb);
            if is_owned && is_base_verb {
                t.merge(cell, MergeOp::RulesRemove(rule.id));
            }
        }
    });
    Ok(())
}
