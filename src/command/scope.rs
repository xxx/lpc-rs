//! What a living's command can see: the neighbourhood (whose rules it may
//! use and what its native captures name), the candidate walk (the parser
//! package's descent through visible containers), and an object's deep
//! contents (`parse_command`'s object scope).

use std::sync::{Arc, Weak};

use lpc_rs_errors::Result;

use crate::{
    command::dispatch::apply_on,
    interpreter::{
        INVENTORY_ACCESSIBLE, INVENTORY_VISIBLE, PARSE_COMMAND_USERS, lpc_ref::LpcRef,
        process::Process, stm::TxnHandle, task_context::TaskContext,
    },
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

/// The objects whose rules `living` may use: itself, its environment, that
/// environment's contents, and its own contents.
pub(crate) fn neighbourhood(txn: &TxnHandle, living: &Arc<Process>) -> Scope {
    let mut members = vec![living.clone()];
    if let Some(environment) = Process::environment_of(txn, living) {
        members.extend(
            Process::inventory_of(txn, &environment)
                .into_iter()
                .filter(|ob| !Arc::ptr_eq(ob, living)),
        );
        members.push(environment);
    }
    members.extend(Process::inventory_of(txn, living));
    Scope::new(members)
}

/// The scope `mover` will have once it stands in `new_env`.
pub(crate) fn after_move(txn: &TxnHandle, mover: &Arc<Process>, new_env: &Arc<Process>) -> Scope {
    let mut members = vec![mover.clone(), new_env.clone()];
    members.extend(Process::inventory_of(txn, new_env));
    members.extend(Process::inventory_of(txn, mover));
    Scope::new(members)
}

/// One candidate and whether the actor can reach it.
#[derive(Clone, Debug)]
pub(crate) struct Candidate {
    pub(crate) object: Arc<Process>,
    pub(crate) reachable: bool,
}

/// The default scope: the actor, its environment, both inventories, and
/// breadth-first the contents of every candidate answering
/// `inventory_visible()`; reachable while every container on the path
/// answers `inventory_accessible()`.
pub(crate) async fn walk(ctx: &TaskContext, actor: &Arc<Process>) -> Result<Vec<Candidate>> {
    let txn = ctx.txn();
    let environment = Process::environment_of(txn, actor);
    let mut out: Vec<Candidate> = vec![Candidate {
        object: actor.clone(),
        reachable: true,
    }];
    if let Some(room) = &environment {
        out.push(Candidate {
            object: room.clone(),
            reachable: true,
        });
        for item in Process::inventory_of(txn, actor) {
            out.push(Candidate {
                object: item,
                reachable: true,
            });
        }
        for item in Process::inventory_of(txn, room) {
            if !Arc::ptr_eq(&item, actor) {
                out.push(Candidate {
                    object: item,
                    reachable: true,
                });
            }
        }
    } else {
        for item in Process::inventory_of(txn, actor) {
            out.push(Candidate {
                object: item,
                reachable: true,
            });
        }
    }
    // Containers are asked as they are reached; a container's contents are
    // reachable only if the container and its path are.
    let mut next = if environment.is_some() { 2 } else { 1 };
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
    let Some(function) = master
        .program
        .unmangled_functions
        .get(PARSE_COMMAND_USERS)
        .cloned()
    else {
        return Ok(Vec::new());
    };
    let value = apply_on(ctx, &master, actor, function, &[]).await?;
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
    let Some(function) = target.program.unmangled_functions.get(name).cloned() else {
        return Ok(true);
    };
    let value = apply_on(ctx, target, actor, function, &[]).await?;
    Ok(value.is_truthy(ctx.txn()))
}

/// `root`, then its inventory breadth-first, each object once.
pub(crate) fn deep(txn: &TxnHandle, root: &Arc<Process>) -> Vec<Arc<Process>> {
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
