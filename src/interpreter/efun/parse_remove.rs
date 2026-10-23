//! `parse_remove`: drop `this_object()`'s rules for a verb.

use lpc_rs_errors::Result;

use crate::{
    command::{frontend::parser::normalize_verb, registry::VerbRules},
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `parse_remove(verb)`: drops every rule `this_object()` registered whose
/// base verb (`protocol().verb`) is `verb`, synonyms included, and purges
/// any rule left behind by a destructed owner.
pub fn parse_remove<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::String(verb) = context.arg(0).clone() else {
        return Err(context.runtime_error("parse_remove: the verb must be a string"));
    };
    let verb = normalize_verb(verb.to_str())
        .map_err(|error| context.runtime_error(format!("parse_remove: {error}")))?;
    VerbRules::new(context.task_context()).remove_verb(context.process(), verb.as_str());
    Ok(())
}
