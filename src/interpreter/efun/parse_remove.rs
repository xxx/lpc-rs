//! `parse_remove`: drop `this_object()`'s rules for a verb.

use lpc_rs_errors::Result;

use crate::{
    command::registry::VerbRules,
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `parse_remove(verb)`: drops every rule `this_object()` registered whose
/// base verb (`protocol().verb`) is `verb`, synonyms included, and purges
/// any rule left behind by a destructed owner.
pub fn parse_remove<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::String(verb) = context.arg(0).clone() else {
        return Err(context.runtime_error("parse_remove: the verb must be a string"));
    };
    VerbRules::new(context.task_context()).remove_verb(context.process(), verb.to_str());
    Ok(())
}
