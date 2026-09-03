//! `parse_remove`: drop `this_object()`'s rules for a verb.

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::registry::VerbRules,
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `parse_remove(verb)`: drops every rule `this_object()` registered whose
/// base verb (`protocol().verb`) is `verb`, synonyms included, and purges
/// any rule left behind by a destructed owner.
pub fn parse_remove<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::String(verb) = context.resolve_local_register(1 as RegisterSize).clone() else {
        return Err(context.runtime_error("parse_remove: the verb must be a string"));
    };
    let this = context.frame().process.clone();
    let verb = verb.to_str();
    let verb_rules = VerbRules::new(context.task_context());
    for rule in verb_rules.all().iter() {
        if rule.owner().is_none() {
            verb_rules.remove(rule.id);
            continue;
        }
        let is_owned = rule.owned_by(&this);
        let is_base_verb = rule.protocol().is_some_and(|p| p.verb.as_str() == verb);
        if is_owned && is_base_verb {
            verb_rules.remove(rule.id);
        }
    }
    Ok(())
}
