//! `parse_sentence`: run a line for `this_player()` over the parser
//! package's verb-attached rules only.

use lpc_rs_errors::Result;

use crate::{
    command::{parser::Nickname, scope::SuppliedScope, trial},
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `parse_sentence(line, ignored, scope, nicknames)`: `1`/`0`/`-1`/`-2`/`-3`,
/// or the master's message, from the parser package's verb-attached rules
/// only — no pre-hook, no fallback, and nothing delivered.
pub async fn parse_sentence<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(actor) = context.this_player().load_full() else {
        return Err(context.runtime_error("parse_sentence: this_player() is not a living"));
    };
    if !actor.is_live(context.txn()) || !actor.commands_enabled(context.txn()) {
        return Err(context.runtime_error("parse_sentence: this_player() is not a living"));
    }

    let LpcRef::String(line) = context.arg(0).clone() else {
        return Err(context.runtime_error("parse_sentence: the line must be a string"));
    };
    let scope = scope_arg(context)?;
    let nicknames = nicknames_arg(context)?;
    let mut task_context = context.task_context().clone();
    task_context.callers = Some(context.chain());

    // Boxed to stay out of `call_efun`'s unboxed future union, which every
    // efun call pays for.
    let outcome = Box::pin(trial::sentence(
        task_context,
        &actor,
        line.to_str(),
        scope,
        &nicknames,
    ))
    .await?;
    context.return_efun_result(outcome.into_value(context.txn()));
    Ok(())
}

/// Register 3: a snapshot of the supplied hierarchy, or the default walk.
fn scope_arg<const N: usize>(context: &EfunContext<'_, N>) -> Result<Option<SuppliedScope>> {
    let Some(value) = context.try_arg(2).filter(|r| !r.is_null_in(context.txn())) else {
        return Ok(None);
    };
    match value {
        LpcRef::Array(_) => SuppliedScope::snapshot(context.txn(), value)
            .map(Some)
            .map_err(|error| error.with_span(context.call_site_span())),
        _ => Err(context.runtime_error("parse_sentence: the scope must be an array of objects")),
    }
}

/// Register 4: empty when absent or `NULL`; a mapping's `(string, object)`
/// entries as nicknames, other entries skipped; anything else is an error.
fn nicknames_arg<const N: usize>(context: &EfunContext<'_, N>) -> Result<Vec<Nickname>> {
    let Some(value) = context.try_arg(3).filter(|r| !r.is_null_in(context.txn())) else {
        return Ok(Vec::new());
    };
    let txn = context.txn();
    match value {
        LpcRef::Mapping(_) => value.with_mapping(txn, |m| {
            m.iter()
                .filter_map(|(key, val)| {
                    let LpcRef::String(name) = key else {
                        return None;
                    };
                    let object = val.live_object(txn)?;
                    Some(Nickname {
                        name: name.to_string(),
                        object,
                    })
                })
                .collect()
        }),
        _ => Err(context
            .runtime_error("parse_sentence: nicknames must be a mapping from strings to objects")),
    }
}
