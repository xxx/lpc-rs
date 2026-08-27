//! `parse_add_synonym`: register another verb for an existing rule.

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::registry::{Frontend, Rule, VerbMatch},
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, stm::MergeOp},
};

/// `parse_add_synonym(new_verb, old_verb, rule)`: copies every rule
/// `this_object()` registered for `old_verb` (or only the one matching
/// `rule`, if given) under `new_verb`, with a fresh id so `parse_remove`
/// can drop the synonym independently. `old_verb` is matched against the
/// rule's own (typed) verb, not its `ParserRule`'s base verb, so a synonym
/// of a synonym works: `parse_add_synonym("gv", "g")` finds the `"g"`
/// synonym after `parse_add_synonym("g", "give")` registered it.
pub async fn parse_add_synonym<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let (LpcRef::String(new_verb), LpcRef::String(old_verb)) = (
        context.resolve_local_register(1 as RegisterSize).clone(),
        context.resolve_local_register(2 as RegisterSize).clone(),
    ) else {
        return Err(context.runtime_error("parse_add_synonym: the verbs must be strings"));
    };
    let rule_filter = context
        .try_resolve_local_register(3 as RegisterSize)
        .filter(|r| !r.is_null())
        .map(|r| match r {
            LpcRef::String(s) => Ok(s.to_str()),
            _ => Err(context.runtime_error("parse_add_synonym: the rule must be a string")),
        })
        .transpose()?;

    let this = context.frame().process.clone();
    let old_verb = old_verb.to_str();
    let cell = context.object_space().verb_rules.id;
    let found: Vec<Rule> = context
        .txn()
        .with(|t| t.read_rules(cell))
        .iter()
        .filter(|r| r.owned_by(&this))
        .filter(|r| r.verb.as_str() == old_verb)
        .filter(|r| match rule_filter {
            None => true,
            Some(wanted) => r
                .handler
                .protocol()
                .is_some_and(|p| p.rule.as_str() == wanted),
        })
        .cloned()
        .collect();
    if found.is_empty() {
        return Err(context.runtime_error(format!(
            "parse_add_synonym: this_object() has no rules for '{old_verb}'"
        )));
    }

    context.txn().with(|t| {
        for rule in &found {
            let synonym = Rule::new(
                &this,
                new_verb.to_str().into(),
                VerbMatch::Exact,
                rule.grammar.clone(),
                rule.handler.clone(),
                Frontend::Parser,
            );
            t.merge(cell, MergeOp::RulesAppend(synonym));
        }
    });
    Ok(())
}
