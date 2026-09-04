//! `parse_sentence`: run a line for `this_player()` over the parser
//! package's verb-attached rules only.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::{
        parser::Nickname,
        trial::{self, Sentence},
    },
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process},
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

    // Boxed to stay out of `call_efun`'s unboxed future union, which every
    // efun call pays for.
    let outcome = Box::pin(trial::sentence(
        context.task_context(),
        &actor,
        line.to_str(),
        scope,
        &nicknames,
    ))
    .await?;
    context.return_efun_result(match outcome {
        Sentence::Handled => LpcRef::from(1),
        Sentence::NoVerb => LpcRef::from(0),
        Sentence::NoParse => LpcRef::from(-1),
        Sentence::Refused => LpcRef::from(-2),
        Sentence::Unresolved => LpcRef::from(-3),
        Sentence::Message(message) => LpcRef::from(message.as_str()),
    });
    Ok(())
}

/// Nested-array depth cap for the scope argument, as `dump`'s
/// `MAX_RECURSION`; a self-referential array is otherwise unbounded.
const MAX_SCOPE_DEPTH: usize = 20;

/// Register 3: `None` when absent or `NULL`; an array's live objects, with
/// nested arrays flattened; anything else is an error.
fn scope_arg<const N: usize>(context: &EfunContext<'_, N>) -> Result<Option<Vec<Arc<Process>>>> {
    let Some(value) = context.try_arg(2).filter(|r| !r.is_null()) else {
        return Ok(None);
    };
    match value {
        LpcRef::Array(_) => flatten_objects(context, value, 0).map(Some),
        _ => Err(context.runtime_error("parse_sentence: the scope must be an array of objects")),
    }
}

/// The live objects of `value`'s array, recursing into nested arrays;
/// `depth` is this array's nesting below the scope argument itself.
fn flatten_objects<const N: usize>(
    context: &EfunContext<'_, N>,
    value: &LpcRef,
    depth: usize,
) -> Result<Vec<Arc<Process>>> {
    if depth > MAX_SCOPE_DEPTH {
        return Err(context.runtime_error(format!(
            "parse_sentence: the scope nests deeper than {MAX_SCOPE_DEPTH}"
        )));
    }
    let txn = context.txn();
    let items: Vec<LpcRef> = value.with_array(txn, |a| a.iter().cloned().collect())?;
    let mut out = Vec::new();
    for item in items {
        match item {
            LpcRef::Array(_) => out.extend(flatten_objects(context, &item, depth + 1)?),
            other => {
                if let Some(object) = other.live_object(txn) {
                    out.push(object);
                }
            }
        }
    }
    Ok(out)
}

/// Register 4: empty when absent or `NULL`; a mapping's `(string, object)`
/// entries as nicknames, other entries skipped; anything else is an error.
fn nicknames_arg<const N: usize>(context: &EfunContext<'_, N>) -> Result<Vec<Nickname>> {
    let Some(value) = context.try_arg(3).filter(|r| !r.is_null()) else {
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
