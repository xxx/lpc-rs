//! Which objects a parser rule may name: the actor's surroundings, walked
//! through `inventory_visible`, reach through `inventory_accessible`, and
//! the livings the master lists.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::dispatch::apply_on,
    interpreter::{
        INVENTORY_ACCESSIBLE, INVENTORY_VISIBLE, PARSE_COMMAND_USERS, lpc_ref::LpcRef,
        process::Process, task_context::TaskContext,
    },
};

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
    let mut out: Vec<Candidate> = vec![Candidate {
        object: actor.clone(),
        reachable: true,
    }];
    if let Some(room) = Process::environment_of(txn, actor) {
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
        for item in Process::inventory_of(txn, &room) {
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
    let mut next = if Process::environment_of(txn, actor).is_some() {
        2
    } else {
        1
    };
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
