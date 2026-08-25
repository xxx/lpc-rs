//! Compilers from each family's surface syntax into the engine's grammars
//! and the registry's rules.

pub mod add_action;

use crate::{
    command::{
        grammar::Parse,
        registry::{Frontend, Rule},
    },
    interpreter::{lpc_ref::LpcRef, lpc_string::LpcString},
};

/// The handler's arguments and the verb `query_verb()` reports, built the way
/// the frontend that registered `rule` builds them.
pub(crate) fn arguments_and_verb(rule: &Rule, parse: &Parse, line: &str) -> (Vec<LpcRef>, String) {
    match rule.source {
        Frontend::AddAction => {
            let verb = rule.verb.as_str();
            let argument = add_action::argument(verb, rule.matching, parse, line);
            (
                vec![LpcString::from(argument.as_str()).into()],
                add_action::reported_verb(verb, rule.matching, parse, line),
            )
        }
    }
}
