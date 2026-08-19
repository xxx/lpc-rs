use std::{sync::Arc, thread::JoinHandle};

use bit_set::BitSet;
use derive_builder::Builder;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::sync::{Mutex, mpsc::Sender};
use tracing::instrument;

use crate::interpreter::{
    call_outs::CallOuts,
    gc::{gc_bank::GcRefBank, mark::Mark, sweep::Sweep},
    object_space::ObjectSpace,
    stm::{CommitProtocol, Committer, Snapshot},
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

    /// All upvalues are stored in the [`Vm`], and are shared between all [`Task`](crate::interpreter::task::Task)s
    #[builder(default, setter(into))]
    upvalues: Arc<RwLock<GcRefBank>>,

    /// The [`Config`] that's in use for this [`Vm`]
    #[builder(default, setter(into))]
    pub config: Arc<Config>,

    /// Enqueued call outs
    #[builder(
        default = "RwLock::new(CallOuts::new(self.tx.clone().unwrap()))",
        setter(into)
    )]
    call_outs: RwLock<CallOuts>,

    /// Global interpreter lock
    #[builder(default)]
    pub gil: Mutex<()>,

    /// The channel used to send [`VmOp`]s to the [`Vm`]
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
            upvalues: Arc::new(RwLock::new(GcRefBank::default())),
            config: conf,
            call_outs: RwLock::new(CallOuts::new(tx.clone())),
            gil: Mutex::new(()),
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

    #[instrument(skip(self))]
    #[inline]
    pub fn sweep(&self, marked: &BitSet) -> Result<()> {
        self.with_upvalues_mut(|g| g.sweep(marked))
    }

    pub fn with_call_outs<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&CallOuts) -> R,
    {
        f(&self.call_outs.read())
    }

    pub fn with_call_outs_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut CallOuts) -> R,
    {
        f(&mut self.call_outs.write())
    }

    pub fn with_upvalues<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&GcRefBank) -> R,
    {
        f(&self.upvalues.read())
    }

    pub fn with_upvalues_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut GcRefBank) -> R,
    {
        f(&mut self.upvalues.write())
    }

    pub fn clone_upvalues(&self) -> Arc<RwLock<GcRefBank>> {
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
        // TODO: mark all tasks
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
