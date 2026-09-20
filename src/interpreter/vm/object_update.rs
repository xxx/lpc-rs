//! Committed update jobs share ownership, status retention and transaction execution.

use std::{
    collections::BTreeMap,
    sync::{
        Arc, Weak,
        atomic::{AtomicI64, Ordering},
    },
};

use lpc_rs_errors::{LpcError, Result};
use parking_lot::Mutex;

use super::{
    global_state::GlobalState, object_recompile::RecompileTarget, system_reload::ReloadTarget,
};
use crate::interpreter::{
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::{
        AttemptBody, CommitProtocol, Conflict, Effect, LiveSnapshot, Transaction, TxnHandle,
        commit_changeset, flush_effects, start_txn,
    },
    task::task_template::TaskTemplate,
};

#[derive(Debug, Clone)]
pub(crate) enum UpdateTarget {
    Recompile(RecompileTarget),
    Restart(ReloadTarget),
}

/// An administrative update delivered after the requesting transaction commits.
#[derive(Debug)]
pub struct UpdateRequest {
    pub(crate) id: i64,
    pub(crate) target: UpdateTarget,
    pub(crate) caller: Weak<Process>,
    pub(crate) player: Option<Weak<Process>>,
    pub(crate) program: Option<String>,
}

impl UpdateRequest {
    pub(crate) fn args(&self, target: LpcRef) -> [LpcRef; 3] {
        [
            target,
            self.caller.clone().into(),
            self.program.as_deref().map(LpcRef::from).unwrap_or(NULL),
        ]
    }
}

#[derive(Debug, Clone)]
pub(crate) struct UpdateStatus {
    pub request: Arc<UpdateRequest>,
    pub state: &'static str,
    pub error: String,
    pub updated: usize,
}

#[derive(Debug, Default)]
pub(crate) struct Updates {
    next: AtomicI64,
    entries: Mutex<BTreeMap<i64, UpdateStatus>>,
}

impl Updates {
    pub(crate) fn mint_id(&self) -> Result<i64> {
        self.next
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |n| n.checked_add(1))
            .map(|n| n + 1)
            .map_err(|_| LpcError::runtime("object update request IDs exhausted"))
    }

    pub(crate) fn enqueue(&self, request: Arc<UpdateRequest>) {
        self.entries.lock().insert(
            request.id,
            UpdateStatus {
                request,
                state: "queued",
                error: String::new(),
                updated: 0,
            },
        );
    }

    pub(crate) fn get(&self, id: i64) -> Option<UpdateStatus> {
        self.entries.lock().get(&id).cloned()
    }

    fn start(&self, id: i64) -> bool {
        let mut entries = self.entries.lock();
        let Some(status) = entries.get_mut(&id) else {
            return false;
        };
        if status.state != "queued" {
            return false;
        }
        status.state = "running";
        true
    }

    pub(crate) fn finish(&self, id: i64, result: Result<usize>) {
        let mut entries = self.entries.lock();
        if let Some(status) = entries.get_mut(&id) {
            match result {
                Ok(updated) => {
                    status.state = "succeeded";
                    status.updated = updated;
                }
                Err(error) => {
                    status.state = "failed";
                    status.error = error.diagnostic_string();
                }
            }
        }
        let finished: Vec<_> = entries
            .iter()
            .filter(|(_, s)| matches!(s.state, "succeeded" | "failed"))
            .map(|(&id, _)| id)
            .collect();
        for id in finished.iter().take(finished.len().saturating_sub(128)) {
            entries.remove(id);
        }
    }
}

impl GlobalState {
    /// Run a committed update once, retrying all preparation on conflict.
    pub async fn run_object_update(self: &Arc<Self>, request: Arc<UpdateRequest>) {
        if !self.updates.start(request.id) {
            return;
        }
        let mut body = UpdateBody {
            gs: self,
            request: &request,
            txn: None,
            updated: 0,
        };
        let (result, _) = self.attempt_runner.run(&mut body).await;
        self.updates
            .finish(request.id, result.map(|()| body.updated));
    }
}

pub(super) struct UpdateBody<'a> {
    pub(super) gs: &'a Arc<GlobalState>,
    pub(super) request: &'a UpdateRequest,
    pub(super) txn: Option<TxnHandle>,
    pub(super) updated: usize,
}

#[async_trait::async_trait]
impl AttemptBody for UpdateBody<'_> {
    fn timeout_ms(&self) -> u64 {
        self.gs.config.max_execution_time
    }

    fn take_compilation_time(&mut self) -> std::time::Duration {
        self.txn.as_ref().map_or(std::time::Duration::ZERO, |txn| {
            txn.with(|t| t.take_compilation_time())
        })
    }

    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = start_txn(tx).await?;
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        self.txn = Some(txn.clone());
        self.updated = 0;
        let caller = self
            .request
            .caller
            .upgrade()
            .ok_or_else(|| LpcError::runtime("object update: requester is gone"))?;
        txn.with(|t| {
            t.set_origin(&caller, "object_update");
            t.reload_preparing = true;
        });
        let mut template = TaskTemplate::from(self.gs.clone());
        template.txn = txn;
        template.set_this_player(self.request.player.as_ref().and_then(Weak::upgrade));
        let mut ctx = template.into_task_context(caller);
        if !ctx.process.is_live(ctx.txn()) {
            return Err(LpcError::runtime("object update: requester is retired"));
        }
        if self.request.player.is_some()
            && !ctx
                .this_player
                .load_full()
                .is_some_and(|p| p.is_live(ctx.txn()))
        {
            return Err(LpcError::runtime(
                "object update: original command giver is gone",
            ));
        }
        self.updated = match &self.request.target {
            UpdateTarget::Recompile(target) => {
                super::object_recompile::prepare(&mut ctx, self.request, target).await?
            }
            UpdateTarget::Restart(target) => {
                super::system_reload::prepare(&mut ctx, self.request, *target).await?
            }
        };
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
        let txn = self.txn.as_ref().expect("attempt opened");
        let result = commit_changeset(tx, txn.with(|t| t.take_changeset())).await?;
        Ok((result, txn.with(|t| t.take_effects())))
    }

    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        flush_effects(self.gs, effects).await;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn status_history_retains_pending_requests_and_bounds_finished_requests() {
        let updates = Updates::default();
        for id in 1..=132 {
            updates.enqueue(Arc::new(UpdateRequest {
                id,
                target: if id % 2 == 0 {
                    UpdateTarget::Restart(ReloadTarget::Master)
                } else {
                    UpdateTarget::Recompile(RecompileTarget::System(ReloadTarget::Master))
                },
                caller: Weak::new(),
                player: None,
                program: None,
            }));
            if id != 1 {
                updates.finish(id, Ok(1));
            }
        }
        assert_eq!(updates.get(1).unwrap().state, "queued");
        assert!(updates.get(2).is_none());
        assert!(updates.get(4).is_none());
        assert!(updates.get(5).is_some());
        assert_eq!(updates.entries.lock().len(), 129);
    }
}
