//! What a living's command can see: the neighbourhood (whose rules it may
//! use and what its native captures name), the candidate walk (the parser
//! package's descent through visible containers), and an object's deep
//! contents (`parse_command`'s object scope).

use std::sync::{Arc, Weak};

use lpc_rs_errors::Result;

use crate::interpreter::{
    INVENTORY_ACCESSIBLE, INVENTORY_VISIBLE, PARSE_COMMAND_USERS, apply::apply_hook,
    lpc_ref::LpcRef, process::Process, stm::TxnHandle, task_context::TaskContext,
};

/// The objects whose rules a living may use, as weak references.
#[derive(Clone, Debug, Default)]
pub struct Scope(Vec<Weak<Process>>);

impl Scope {
    /// A scope over `members`, held weakly.
    pub fn new(members: impl IntoIterator<Item = Arc<Process>>) -> Scope {
        Scope(members.into_iter().map(|p| Arc::downgrade(&p)).collect())
    }

    /// Whether `process` is a member, by pointer identity.
    pub fn contains(&self, process: &Arc<Process>) -> bool {
        self.0
            .iter()
            .any(|w| std::ptr::eq(w.as_ptr(), Arc::as_ptr(process)))
    }

    /// Whether `owner` is a member, by pointer identity.
    pub fn contains_weak(&self, owner: &Weak<Process>) -> bool {
        self.0.iter().any(|w| Weak::ptr_eq(w, owner))
    }

    /// The members not yet dropped, in scope order.
    pub fn members(&self) -> Vec<Arc<Process>> {
        self.0.iter().filter_map(Weak::upgrade).collect()
    }
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len()
            && self.0.iter().zip(&other.0).all(|(a, b)| Weak::ptr_eq(a, b))
    }
}

/// `living`, `environment` when there is one, `living`'s own contents,
/// then `environment`'s other contents — the one order every enumeration
/// of a command's surroundings uses — with the count of leading entries
/// that are never asked about their contents (the living and the room).
fn around(
    txn: &TxnHandle,
    living: &Arc<Process>,
    environment: Option<&Arc<Process>>,
) -> (Vec<Arc<Process>>, usize) {
    let mut out = vec![living.clone()];
    out.extend(environment.cloned());
    out.extend(Process::inventory_of(txn, living));
    if let Some(environment) = environment {
        out.extend(
            Process::inventory_of(txn, environment)
                .into_iter()
                .filter(|item| !Arc::ptr_eq(item, living)),
        );
    }
    (out, 1 + usize::from(environment.is_some()))
}

/// The objects whose rules `living` may use and whose ids its native
/// captures resolve over, in `around` order.
pub(crate) fn neighbourhood(txn: &TxnHandle, living: &Arc<Process>) -> Scope {
    let environment = Process::environment_of(txn, living);
    Scope::new(around(txn, living, environment.as_ref()).0)
}

/// The neighbourhood `mover` will have once it stands in `new_env`, from
/// the inventories as they are before the move.
pub(crate) fn after_move(txn: &TxnHandle, mover: &Arc<Process>, new_env: &Arc<Process>) -> Scope {
    Scope::new(around(txn, mover, Some(new_env)).0)
}

/// One candidate and whether the actor can reach it.
#[derive(Clone, Debug)]
pub(crate) struct Candidate {
    pub(crate) object: Arc<Process>,
    pub(crate) reachable: bool,
}

/// The candidate walk: the neighbourhood, then breadth-first the contents
/// of every candidate answering `inventory_visible()`; reachable while
/// every container on the path answers `inventory_accessible()`.
pub(crate) async fn walk(ctx: &TaskContext, actor: &Arc<Process>) -> Result<Vec<Candidate>> {
    let txn = ctx.txn();
    let environment = Process::environment_of(txn, actor);
    let (members, mut next) = around(txn, actor, environment.as_ref());
    let mut out: Vec<Candidate> = members
        .into_iter()
        .map(|object| Candidate {
            object,
            reachable: true,
        })
        .collect();
    while next < out.len() {
        let holder = out[next].clone();
        next += 1;
        if !truthy(ctx, actor, &holder.object, INVENTORY_VISIBLE).await? {
            continue;
        }
        let reachable =
            holder.reachable && truthy(ctx, actor, &holder.object, INVENTORY_ACCESSIBLE).await?;
        for item in Process::inventory_of(txn, &holder.object) {
            if !out.iter().any(|seen| Arc::ptr_eq(&seen.object, &item)) {
                out.push(Candidate {
                    object: item,
                    reachable,
                });
            }
        }
    }
    Ok(out)
}

/// `master->parse_command_users()`'s livings; empty without the apply.
pub(crate) async fn users(ctx: &TaskContext, actor: &Arc<Process>) -> Result<Vec<Arc<Process>>> {
    let Some(master) = ctx.object_space().master_object() else {
        return Ok(Vec::new());
    };
    let Some(value) = apply_hook(ctx, &master, actor, PARSE_COMMAND_USERS, &[]).await? else {
        return Ok(Vec::new());
    };
    match &value {
        LpcRef::Array(_) => value.with_array(ctx.txn(), |a| {
            a.iter()
                .filter_map(|item| item.live_object(ctx.txn()))
                .filter(|object| object.commands_enabled(ctx.txn()))
                .collect()
        }),
        _ => Ok(Vec::new()),
    }
}

/// Whether `target->name()` is truthy; an undefined apply is truthy — a
/// plain object hides nothing.
async fn truthy(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    target: &Arc<Process>,
    name: &str,
) -> Result<bool> {
    let Some(value) = apply_hook(ctx, target, actor, name, &[]).await? else {
        return Ok(true);
    };
    Ok(value.is_truthy(ctx.txn()))
}

/// `root`, then its contents depth-first — a container's contents right
/// behind it — each object once, so a containment cycle ends.
pub(crate) fn deep(txn: &TxnHandle, root: &Arc<Process>) -> Vec<Arc<Process>> {
    let mut out = vec![root.clone()];
    descend(txn, root, &mut out);
    out
}

/// Append `container`'s contents to `out` depth-first, skipping anything
/// already in it.
fn descend(txn: &TxnHandle, container: &Arc<Process>, out: &mut Vec<Arc<Process>>) {
    for item in Process::inventory_of(txn, container) {
        if out.iter().any(|seen| Arc::ptr_eq(seen, &item)) {
            continue;
        }
        out.push(item.clone());
        descend(txn, &item, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scope_membership_is_by_identity() {
        let a = Arc::new(Process::default());
        let b = Arc::new(Process::default());
        let scope = Scope::new([a.clone()]);
        assert!(scope.contains(&a));
        assert!(!scope.contains(&b));
        assert!(scope.contains_weak(&Arc::downgrade(&a)));
        assert_eq!(scope, Scope::new([a.clone()]));
        assert_ne!(scope, Scope::new([b]));
    }
}
