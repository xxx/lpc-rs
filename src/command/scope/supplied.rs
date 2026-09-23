//! Snapshot and traversal of caller-supplied parser scopes.

use std::{collections::HashMap, sync::Arc};

use lpc_rs_errors::{Result, lpc_error};

use super::{Candidate, truthy};
use crate::interpreter::{
    INVENTORY_ACCESSIBLE, INVENTORY_VISIBLE, lpc_ref::LpcRef, process::Process, stm::TxnHandle,
    task_context::TaskContext,
};

/// Nested-array depth cap, including scopes that contain themselves.
const MAX_SCOPE_DEPTH: usize = 20;

/// One object occurrence in depth-first order, with its supplied parent.
#[derive(Debug)]
struct Entry {
    object: Arc<Process>,
    parent: Option<usize>,
}

/// Immutable array structure shared across a sentence's rule attempts.
#[derive(Clone, Debug)]
pub(crate) struct SuppliedScope(Arc<[Entry]>);

impl SuppliedScope {
    /// Capture parent relationships before any parser callback can mutate arrays.
    pub(crate) fn snapshot(txn: &TxnHandle, array: &LpcRef) -> Result<Self> {
        let mut entries = Vec::new();
        snapshot(txn, array, None, 0, &mut entries)?;
        Ok(Self(entries.into()))
    }

    /// Visible objects in first-occurrence order, reachable through any visible path.
    pub(crate) async fn walk(
        &self,
        ctx: &TaskContext,
        actor: &Arc<Process>,
    ) -> Result<Vec<Candidate>> {
        let mut paths: Vec<Option<bool>> = Vec::with_capacity(self.0.len());
        let mut out: Vec<Candidate> = Vec::new();
        let mut positions: HashMap<usize, usize> = HashMap::new();
        let mut policies = HashMap::new();
        for entry in self.0.iter() {
            tokio::task::consume_budget().await;
            let path = match entry.parent {
                None => Some(true),
                Some(index) => match paths[index] {
                    None => None,
                    Some(reachable) => {
                        let parent = &self.0[index].object;
                        if !parent.is_live(ctx.txn())
                            || !policy(ctx, actor, parent, INVENTORY_VISIBLE, &mut policies).await?
                            || !parent.is_live(ctx.txn())
                        {
                            None
                        } else {
                            let reachable = reachable
                                && policy(ctx, actor, parent, INVENTORY_ACCESSIBLE, &mut policies)
                                    .await?;
                            parent.is_live(ctx.txn()).then_some(reachable)
                        }
                    }
                },
            };
            let path = path.filter(|_| entry.object.is_live(ctx.txn()));
            paths.push(path);
            if let Some(reachable) = path {
                let key = Arc::as_ptr(&entry.object) as usize;
                if let Some(&index) = positions.get(&key) {
                    out[index].reachable |= reachable;
                } else {
                    positions.insert(key, out.len());
                    out.push(Candidate {
                        object: entry.object.clone(),
                        reachable,
                    });
                }
            }
        }
        Ok(out)
    }
}

/// Cache each inventory apply only within this rule's scope construction.
async fn policy(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    object: &Arc<Process>,
    name: &'static str,
    policies: &mut HashMap<(usize, &'static str), bool>,
) -> Result<bool> {
    let key = (Arc::as_ptr(object) as usize, name);
    if let Some(&value) = policies.get(&key) {
        return Ok(value);
    }
    let value = truthy(ctx, actor, object, name).await?;
    policies.insert(key, value);
    Ok(value)
}

fn snapshot(
    txn: &TxnHandle,
    array: &LpcRef,
    parent: Option<usize>,
    depth: usize,
    entries: &mut Vec<Entry>,
) -> Result<()> {
    if depth > MAX_SCOPE_DEPTH {
        return Err(lpc_error!(
            "parse_sentence: the scope nests deeper than {MAX_SCOPE_DEPTH}"
        ));
    }
    let items: Vec<LpcRef> = array.with_array(txn, |a| a.iter().cloned().collect())?;
    let mut previous = None;
    for item in items {
        match item {
            LpcRef::Array(_) => {
                let Some(parent) = previous.take() else {
                    return Err(lpc_error!(
                        "parse_sentence: a nested scope array must immediately follow a live object"
                    ));
                };
                snapshot(txn, &item, Some(parent), depth + 1, entries)?;
            }
            other => {
                previous = other.live_object(txn).map(|object| {
                    let index = entries.len();
                    entries.push(Entry { object, parent });
                    index
                });
            }
        }
    }
    Ok(())
}
