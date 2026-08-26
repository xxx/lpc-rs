//! `parse_command`: match a command line against a pattern, resolve its
//! noun captures against a scope of objects, and write every capture into
//! its by-reference destination.

use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::native::{self, CaptureKind},
        grammar::{Grammar, parse},
        resolve::{LpcVocabulary, Resolver, values},
    },
    interpreter::{
        efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process, stm::TxnHandle,
        task_context::TaskContext,
    },
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
            (*kind == CaptureKind::Preposition)
                .then(|| preposition_list(context, slot))
                .flatten()
        })
        .collect();
    let callers_list = array_lists.iter().flatten().next().cloned();

    // Boxed so the borrowed future does not join the stack of `call_efun`'s
    // single unboxed union, which every other efun's future also shares.
    let matched = Box::pin(resolve_against_scope(
        context.task_context(),
        scope,
        &compiled.grammar,
        cmd,
        callers_list,
    ))
    .await?;
    let Some((found, in_force)) = matched else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };

    for (slot, value) in found.into_iter().enumerate() {
        let value = match (&array_lists[slot], value.as_str()) {
            (Some(_), Some(matched)) => swapped_to_front(context, &in_force, matched),
            _ => value,
        };
        let Some(index) = RegisterSize::try_from(3 + slot).ok() else {
            return Err(context.runtime_bug(format!("capture slot {slot} does not fit a register")));
        };
        context.write_ref(index, value)?;
    }
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

/// Every parse of `cmd` under `grammar`, tried in turn until one's captures
/// all resolve against `scope`'s vocabulary; the resolved values and the
/// preposition list that was in force, or `None` when nothing resolves.
async fn resolve_against_scope(
    ctx: &TaskContext,
    scope: Vec<Arc<Process>>,
    grammar: &Grammar,
    cmd: &str,
    callers_prepositions: Option<Vec<String>>,
) -> Result<Option<(Vec<LpcRef>, Vec<String>)>> {
    let vocabulary = LpcVocabulary::new(ctx, scope);
    let mut resolver = Resolver::new(vocabulary, callers_prepositions).await?;
    for parsed in parse(grammar, cmd) {
        let Some(captures) = native::captures(&parsed) else {
            continue;
        };
        if let Some(found) = values(&captures, &mut resolver).await? {
            return Ok(Some((found, resolver.prepositions().to_vec())));
        }
    }
    Ok(None)
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
            .map(|root| deep_scope(txn, &root))
            .unwrap_or_default()),
        LpcRef::Array(_) => arg.with_array(txn, |a| {
            a.iter().filter_map(|item| item.live_object(txn)).collect()
        }),
        _ => Err(context
            .runtime_error("parse_command: the scope must be an object or an array of objects")),
    }
}

/// `root`, then its inventory breadth-first, each object once.
fn deep_scope(txn: &TxnHandle, root: &Arc<Process>) -> Vec<Arc<Process>> {
    let mut out = vec![root.clone()];
    let mut next = 0;
    while next < out.len() {
        let container = out[next].clone();
        for item in Process::inventory_of(txn, &container) {
            if !out.iter().any(|seen| Arc::ptr_eq(seen, &item)) {
                out.push(item);
            }
        }
        next += 1;
    }
    out
}

/// The string members of the array in `slot`'s destination, or `None` when
/// it holds no array.
fn preposition_list<const N: usize>(
    context: &EfunContext<'_, N>,
    slot: usize,
) -> Option<Vec<String>> {
    let value = context.resolve_local_register(destination(slot)?);
    value
        .with_array(context.txn(), |a| {
            a.iter()
                .filter_map(|item| item.as_str().map(str::to_owned))
                .collect::<Vec<_>>()
        })
        .ok()
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
