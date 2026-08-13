use std::sync::Arc;

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
    pub upvalues: Arc<RwLock<GcRefBank>>,

    /// The [`Config`] that's in use for this [`Vm`]
    #[builder(default, setter(into))]
    pub config: Arc<Config>,

    /// Enqueued call outs
    #[builder(
        default = "RwLock::new(CallOuts::new(self.tx.clone().unwrap()))",
        setter(into)
    )]
    pub call_outs: RwLock<CallOuts>,

    /// Global interpreter lock
    #[builder(default)]
    pub gil: Mutex<()>,

    /// The channel used to send [`VmOp`]s to the [`Vm`]
    pub tx: Sender<VmOp>,
}

impl GlobalState {
    pub fn new<C>(config: C, tx: Sender<VmOp>) -> Self
    where
        C: Into<Arc<Config>>,
    {
        let conf = config.into();

        Self {
            object_space: Arc::new(ObjectSpace::new(conf.clone())),
            upvalues: Arc::new(RwLock::new(GcRefBank::default())),
            config: conf,
            call_outs: RwLock::new(CallOuts::new(tx.clone())),
            gil: Mutex::new(()),
            tx,
        }
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
}

impl Mark for GlobalState {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        // TODO: mark all tasks
        self.object_space.mark(marked, processed)?;

        self.with_call_outs(|co| co.mark(marked, processed))
    }
}
