use std::{sync::Arc, thread::JoinHandle};

use bit_set::BitSet;
use derive_builder::Builder;
use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;
use tracing::instrument;

use crate::interpreter::{
    call_outs::CallOuts,
    gc::{gc_bank::GcVarIdBank, mark::Mark},
    object_space::ObjectSpace,
    stm::{CommitProtocol, Committer, GcReport, Snapshot, VarId, WorldRoot, gc_pass, live_count},
    vm::vm_op::VmOp,
};

/// A type for globally-shared state that every [`Task`](crate::interpreter::task::Task) will need access to.
#[derive(Debug, Builder)]
#[readonly::make]
#[builder(setter(into), pattern = "owned")]
pub struct GlobalState {
    /// Our object space, which stores all of the system objects (masters and clones)
    #[builder(default, setter(into))]
    pub object_space: Arc<ObjectSpace>,

    /// All upvalues are stored in the [`Vm`](crate::interpreter::vm::Vm), and are shared between all [`Task`](crate::interpreter::task::Task)s.
    /// Each slot holds a transactional `VarId`; the committed value lives in the committer's world.
    #[builder(default, setter(into))]
    upvalues: Arc<RwLock<GcVarIdBank>>,

    /// The [`Config`] that's in use for this [`Vm`](crate::interpreter::vm::Vm)
    #[builder(default, setter(into))]
    pub config: Arc<Config>,

    /// Enqueued call outs
    #[builder(
        default = "RwLock::new(CallOuts::new(self.tx.clone().unwrap()))",
        setter(into)
    )]
    call_outs: RwLock<CallOuts>,

    /// The channel used to send [`VmOp`]s to the [`Vm`](crate::interpreter::vm::Vm)
    pub tx: Sender<VmOp>,

    /// Sender to this state's single committer thread.
    pub(crate) committer_tx: flume::Sender<CommitProtocol>,

    /// Handle to the committer thread.
    #[builder(default)]
    committer_handle: Option<JoinHandle<Snapshot>>,
}

impl GlobalState {
    pub fn new<C>(config: C, tx: Sender<VmOp>) -> Self
    where
        C: Into<Arc<Config>>,
    {
        let conf = config.into();
        let (committer_tx, committer_handle) = Self::spawn_committer();

        Self {
            object_space: Arc::new(ObjectSpace::new(conf.clone())),
            upvalues: Arc::new(RwLock::new(GcVarIdBank::default())),
            config: conf,
            call_outs: RwLock::new(CallOuts::new(tx.clone())),
            tx,
            committer_tx,
            committer_handle: Some(committer_handle),
        }
    }

    /// Spawn a committer thread; return its sender and join handle. Used by
    /// `new` and by the few builder-based test fixtures.
    pub(crate) fn spawn_committer() -> (flume::Sender<CommitProtocol>, JoinHandle<Snapshot>) {
        let (tx, rx) = flume::unbounded();
        let committer_tx = tx.clone();
        let handle = std::thread::Builder::new()
            .name("stm-committer".into())
            .spawn(move || Committer::new().run(committer_tx, rx))
            .expect("failed to spawn committer thread");
        (tx, handle)
    }

    /// Tell the committer to shut down. Idempotent: the committer exits on
    /// the first `Close`; later sends just fail (channel closed).
    pub fn close_committer(&self) {
        let _ = self.committer_tx.send(CommitProtocol::Close);
    }

    /// One GC pass, atomic on the committer: refuse unless quiescent, else
    /// cull the unmarked upvalue cells and reclaim the unreachable world
    /// payload vars. The cells travel inside the pass message, so their drops
    /// cannot be reordered past the sweep, and the committer's re-check closes
    /// the gap after the client's fast-refuse probe.
    ///
    /// # Precondition
    ///
    /// The committer must be **quiescent** — a non-quiescent call is refused
    /// rather than blocked: a concurrent task at a GC point is a caller bug.
    ///
    /// The bank cull happens only after the pass commits; a refused pass
    /// leaves the bank untouched.
    #[instrument(skip_all)]
    pub async fn gc(&self) -> Result<GcReport> {
        let live = live_count(&self.committer_tx).await?;
        if live != 0 {
            return Err(lpc_error!(
                "gc refused: committer not quiescent ({live} transaction(s) in flight)"
            ));
        }

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        self.mark(&mut marked, &mut processed)?;

        // The culled cells' ids, computed without mutating the bank; the
        // surviving cells are the world sweep's upvalue roots. The read guard
        // is scoped to this block so it drops before the pass's await.
        let (dropped, survivors): (Vec<VarId>, Vec<VarId>) = {
            let bank = self.upvalues.read();
            (
                bank.iter()
                    .filter(|(idx, _)| !marked.contains(*idx))
                    .map(|(_, id)| *id)
                    .collect(),
                bank.iter()
                    .filter(|(idx, _)| marked.contains(*idx))
                    .map(|(_, id)| *id)
                    .collect(),
            )
        };

        let mut roots: Vec<WorldRoot> = self
            .object_space
            .all_cell_ids()
            .into_iter()
            .map(WorldRoot::Var)
            .collect();
        // Bootstrap objects have no committed cell, so their global slots are
        // rooted directly: `all_cell_ids` alone would wrongly reclaim them.
        self.object_space
            .all_live_object_slots()
            .into_iter()
            .for_each(|id| roots.push(WorldRoot::Var(id)));
        for id in &survivors {
            roots.push(WorldRoot::Var(*id));
        }
        self.with_call_outs(|co| {
            for (_, call_out) in co.queue() {
                roots.push(WorldRoot::Ref(call_out.func_ref.clone()));
            }
        });

        let report = gc_pass(&self.committer_tx, dropped, roots).await?;

        // Cull the bank only after the committer has applied the drops.
        self.with_upvalues_mut(|cells| {
            cells.retain(|_, id| survivors.contains(id));
        });
        Ok(report)
    }

    pub fn with_call_outs<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&CallOuts) -> R,
    {
        f(&self.call_outs.read())
    }

    /// The call-out queue's lock, for flushing deferred scheduling effects
    /// (the flush needs a `&RwLock`, not a closure, because it spans awaits).
    pub(crate) fn call_outs(&self) -> &RwLock<CallOuts> {
        &self.call_outs
    }

    pub fn with_call_outs_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut CallOuts) -> R,
    {
        f(&mut self.call_outs.write())
    }

    /// Read the upvalue bank. Production never reads the cells directly
    /// (value reads route through the committer), so this is test-only.
    #[cfg(test)]
    pub(crate) fn with_upvalues<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&GcVarIdBank) -> R,
    {
        f(&self.upvalues.read())
    }

    pub(crate) fn with_upvalues_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut GcVarIdBank) -> R,
    {
        f(&mut self.upvalues.write())
    }

    pub(crate) fn clone_upvalues(&self) -> Arc<RwLock<GcVarIdBank>> {
        self.upvalues.clone()
    }
}

impl Drop for GlobalState {
    fn drop(&mut self) {
        // Send `Close` so the committer exits even if our sender were the
        // last one (it isn't — the committer holds a clone — but `Close`
        // makes the exit deterministic rather than relying on channel
        // close), then join so we never leak the thread. `let _ =` on the
        // join: a panicked committer (which shouldn't happen — `run`/
        // `process` don't panic) must not take the drop of the whole
        // interpreter state with it.
        self.close_committer();
        if let Some(handle) = self.committer_handle.take() {
            let _ = handle.join();
        }
    }
}

impl Mark for GlobalState {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        // The committer world is not a GC root here: payload vars committed into
        // it are unmarked and the sweep only culls upvalue cells, so a payload
        // reachable only through a committed slot is retained. Reclaiming the
        // dead ones is the quiescent pass's job (`Vm::gc`).
        //
        // No live tasks are marked: the pass runs only at quiescence, when no
        // transaction is in flight.
        self.object_space.mark(marked, processed)?;

        self.with_call_outs(|co| co.mark(marked, processed))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::test_support::test_config;

    #[test]
    fn committer_thread_exits_when_global_state_drops() {
        let (tx, _rx) = tokio::sync::mpsc::channel(8);
        let probe;
        {
            let gs = Arc::new(GlobalState::new(test_config(), tx));
            probe = gs.committer_tx.clone();
        } // last Arc released here -> GlobalState::drop sends Close + joins.
        // If the join ever blocked, this test would hang.
        assert!(
            probe.is_disconnected(),
            "committer channel should be closed after the committer exited"
        );
    }
}
