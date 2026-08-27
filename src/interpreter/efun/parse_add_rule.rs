//! `parse_add_rule`: register a parser-package rule for a verb.

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::parser::compile,
        registry::{Frontend, Handler, Rule, VerbMatch},
    },
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, stm::MergeOp},
};

/// `parse_add_rule(verb, rule)`: appends a verb-attached rule owned by
/// `this_object()`, which must have called `parse_init()`.
pub async fn parse_add_rule<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let this = context.frame().process.clone();
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
    let grammar = parser.compiled.grammar.clone();
    let rule = Rule::new(
        &this,
        verb.to_str().into(),
        VerbMatch::Exact,
        grammar,
        Handler::Protocol(std::sync::Arc::new(parser)),
        Frontend::Parser,
    );
    let cell = context.object_space().verb_rules.id;
    context.txn().with(|t| {
        for dead in t.read_rules(cell).iter().filter(|r| r.owner().is_none()) {
            t.merge(cell, MergeOp::RulesRemove(dead.id));
        }
        t.merge(cell, MergeOp::RulesAppend(rule));
    });
    Ok(())
}
