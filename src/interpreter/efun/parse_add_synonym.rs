//! `parse_add_synonym`: register another verb for an existing rule.

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::registry::{Rule, VerbRules},
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `parse_add_synonym(new_verb, old_verb, rule)`: copies every rule
/// `this_object()` registered under `old_verb` (or only the one whose text
/// is `rule`) as a fresh rule for `new_verb` sharing its handlers; `old_verb`
/// is the typed verb, so a synonym of a synonym works.
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
    let verb_rules = VerbRules::new(context.task_context());
    let found: Vec<Rule> = verb_rules
        .owned_by(&this)
        .into_iter()
        .filter(|r| r.verb.as_str() == old_verb)
        .filter(|r| match rule_filter {
            None => true,
            Some(wanted) => r.protocol().is_some_and(|p| p.rule.as_str() == wanted),
        })
        .collect();
    if found.is_empty() {
        return Err(context.runtime_error(format!(
            "parse_add_synonym: this_object() has no rules for '{old_verb}'"
        )));
    }
    for rule in &found {
        verb_rules.append(Rule::new(
            &this,
            new_verb.to_str().into(),
            rule.family.clone(),
        ));
    }
    Ok(())
}
