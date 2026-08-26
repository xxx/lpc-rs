//! Compilers from each family's surface syntax into the engine's grammars
//! and the registry's rules, and each family's handler arguments back out
//! of a parse.

pub mod add_action;
pub mod dgd;
pub mod native;

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::{
        grammar::parse,
        registry::{Frontend, Rule},
        resolve::{LpcVocabulary, Resolver, values},
    },
    interpreter::{
        lpc_ref::LpcRef, lpc_string::LpcString, process::Process, task_context::TaskContext,
    },
};

/// The handler's arguments and the verb `query_verb()` reports for the
/// first parse of `line` that yields usable arguments — for a native rule,
/// one whose noun captures resolve against `scope`; `None` when no parse
/// does, which makes the rule no match. The resolver is built on first
/// need and shared across the trial.
pub(crate) async fn arguments_and_verb<'a>(
    ctx: &'a TaskContext,
    scope: &[Arc<Process>],
    resolver: &mut Option<Resolver<LpcVocabulary<'a>>>,
    rule: &Rule,
    line: &str,
) -> Result<Option<(Vec<LpcRef>, String)>> {
    match rule.source {
        Frontend::AddAction => {
            let Some(parsed) = parse(&rule.grammar, line).next() else {
                return Ok(None);
            };
            let verb = rule.verb.as_str();
            let argument = add_action::argument(verb, rule.matching, &parsed, line);
            Ok(Some((
                vec![LpcString::from(argument.as_str()).into()],
                add_action::reported_verb(verb, rule.matching, &parsed, line),
            )))
        }
        Frontend::Native => {
            for parsed in parse(&rule.grammar, line) {
                let Some(captures) = native::captures(&parsed) else {
                    continue;
                };
                if captures
                    .iter()
                    .all(|capture| capture.kind.resolver_kind().is_none())
                {
                    let plain: Option<Vec<LpcRef>> =
                        captures.iter().map(native::plain_value).collect();
                    return Ok(plain.map(|args| (args, rule.verb.to_string())));
                }
                let resolver = match resolver {
                    Some(resolver) => resolver,
                    None => {
                        let vocabulary = LpcVocabulary::new(ctx, scope.to_vec());
                        resolver.insert(Resolver::new(vocabulary, None).await?)
                    }
                };
                if let Some(args) = values(&captures, resolver).await? {
                    return Ok(Some((args, rule.verb.to_string())));
                }
            }
            Ok(None)
        }
        // The trial filters a `Protocol` handler out by its own dispatch
        // arm before it ever reaches this call.
        Frontend::Parser => Ok(None),
    }
}
