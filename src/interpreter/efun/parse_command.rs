//! `parse_command`: match a command line against a pattern, resolve its
//! noun captures against a scope of objects, and write every capture into
//! its by-reference destination.

use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::native::{self, CaptureKind},
        resolve::{LpcVocabulary, Resolver},
        scope,
    },
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process},
};

/// The register holding the destination for capture slot `slot`: registers
/// 1–3 are `cmd`, `scope`, `pattern`.
fn destination(slot: usize) -> Option<RegisterSize> {
    RegisterSize::try_from(4 + slot).ok()
}

/// `parse_command(cmd, scope, pattern, ref...)`: 1 and the destinations
/// written when some parse of `cmd` covers the pattern and every noun
/// resolves; 0 and nothing written otherwise.
pub async fn parse_command<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let cmd = context.resolve_local_register(1 as RegisterSize).clone();
    let scope_arg = context.resolve_local_register(2 as RegisterSize).clone();
    let pattern = context.resolve_local_register(3 as RegisterSize).clone();
    let (Some(cmd), Some(pattern)) = (cmd.as_str(), pattern.as_str()) else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };
    if cmd.is_empty() || pattern.is_empty() {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    }
    let scope = scope_of(context, &scope_arg)?;
    let compiled = native::compile_pattern(pattern)
        .map_err(|e| context.runtime_error(format!("parse_command: {e}")))?;
    let destinations = usize::from(context.frame().called_with_num_args).saturating_sub(3);
    if compiled.kinds.len() > destinations {
        return Err(context.runtime_error("parse_command: too few arguments for the pattern"));
    }

    let array_lists: Vec<Option<Vec<String>>> = compiled
        .kinds
        .iter()
        .enumerate()
        .map(|(slot, kind)| {
            if *kind == CaptureKind::Preposition {
                preposition_list(context, slot)
            } else {
                Ok(None)
            }
        })
        .collect::<Result<Vec<_>>>()?;
    let callers_list = array_lists.iter().flatten().next().cloned();

    let has_preposition_slot = array_lists.iter().any(Option::is_some);
    // The resolver's borrow of the task context must end before `context` writes the registers.
    let (found, in_force) = {
        let vocabulary = LpcVocabulary::new(context.task_context(), scope);
        let mut resolver = Resolver::new(vocabulary, callers_list);
        // Boxed to stay out of `call_efun`'s unboxed future union, which every
        // efun call pays for.
        let found = Box::pin(native::arguments(&compiled, cmd, &mut resolver)).await?;
        let in_force: Vec<String> = if found.is_some() && has_preposition_slot {
            resolver.prepositions().await?.to_vec()
        } else {
            Vec::new()
        };
        (found, in_force)
    };
    let Some(found) = found else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };

    for (slot, value) in found.into_iter().enumerate() {
        let value = match (&array_lists[slot], value.as_str()) {
            (Some(_), Some(matched)) => swapped_to_front(context, &in_force, matched),
            _ => value,
        };
        // `write_ref` takes a 0-based argument index (`cmd`, `scope`, `pattern` are 0..3),
        // not the register number `destination` computes for reads.
        let Some(index) = RegisterSize::try_from(3 + slot).ok() else {
            return Err(context.runtime_bug(format!("capture slot {slot} does not fit a register")));
        };
        context.write_ref(index, value)?;
    }
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

/// The candidates `arg` names: an object with its deep inventory, or an
/// array's live object members. A destructed object is an empty scope.
fn scope_of<const N: usize>(
    context: &EfunContext<'_, N>,
    arg: &LpcRef,
) -> Result<Vec<Arc<Process>>> {
    let txn = context.txn();
    match arg {
        LpcRef::Object(_) => Ok(arg
            .live_object(txn)
            .map(|root| scope::deep(txn, &root))
            .unwrap_or_default()),
        LpcRef::Array(_) => arg.with_array(txn, |a| {
            a.iter().filter_map(|item| item.live_object(txn)).collect()
        }),
        _ => Err(context
            .runtime_error("parse_command: the scope must be an object or an array of objects")),
    }
}

/// The string members of the array in `slot`'s destination, or `None` when
/// it holds no array; an array whose contents the world has lost is an
/// error, not `None` — a driver bug must surface.
fn preposition_list<const N: usize>(
    context: &EfunContext<'_, N>,
    slot: usize,
) -> Result<Option<Vec<String>>> {
    let Some(register) = destination(slot) else {
        return Ok(None);
    };
    let value = context.resolve_local_register(register);
    if !matches!(value, LpcRef::Array(_)) {
        return Ok(None);
    }
    Ok(Some(value.with_array(context.txn(), |a| {
        a.iter()
            .filter_map(|item| item.as_str().map(str::to_owned))
            .collect::<Vec<_>>()
    })?))
}

/// A new array of `list` with `matched` swapped into `[0]`, as CD returns a
/// caller's preposition list.
fn swapped_to_front<const N: usize>(
    context: &EfunContext<'_, N>,
    list: &[String],
    matched: &str,
) -> LpcRef {
    let mut list: Vec<&str> = list.iter().map(String::as_str).collect();
    if let Some(index) = list.iter().position(|entry| *entry == matched) {
        list.swap(0, index);
    }
    context.mint_array(list.into_iter().map(LpcRef::from))
}
