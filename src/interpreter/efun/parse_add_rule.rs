//! `parse_add_rule`: register a parser-package rule for a verb.

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::parser::compile,
        registry::{Family, Rule, VerbRules},
    },
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `parse_add_rule(verb, rule)`: appends a verb-attached rule owned by
/// `this_object()`, which must have called `parse_init()`. Do not read
/// `verb_rules` here — it would make parallel registrations conflict; dead
/// owners are purged by `parse_remove` and destruct.
pub fn parse_add_rule<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let this = context.process().clone();
    if this.parser_ready.get().is_none() {
        return Err(context.runtime_error("parse_add_rule: parse_init() has not been called"));
    }
    let (LpcRef::String(verb), LpcRef::String(rule)) = (
        context.resolve_local_register(1 as RegisterSize).clone(),
        context.resolve_local_register(2 as RegisterSize).clone(),
    ) else {
        return Err(context.runtime_error("parse_add_rule: the verb and rule must be strings"));
    };
    let parser = compile(verb.to_str(), rule.to_str())
        .map_err(|e| context.runtime_error(format!("parse_add_rule: {e}")))?;
    let rule = Rule::new(
        &this,
        verb.to_str().into(),
        Family::Parser(std::sync::Arc::new(parser)),
    );
    VerbRules::new(context.task_context()).append(rule);
    Ok(())
}
