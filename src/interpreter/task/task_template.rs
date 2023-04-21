use std::{sync::Arc};

use derive_builder::Builder;

use lpc_rs_utils::config::Config;

use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;

use crate::{
    interpreter::{
        call_outs::CallOuts, gc::gc_bank::GcRefBank, heap::Heap,
        object_space::ObjectSpace, vm::vm_op::VmOp,
    },
};

/// A struct to handle the non-changing Task state, so we can prepare it ahead of time.
#[derive(Debug, Clone, Builder)]
pub struct TaskTemplate {
    /// The [`Config`] that's in use for the
    /// [`Task`](crate::interpreter::task::Task).
    #[builder(setter(into))]
    pub config: Arc<Config>,

    /// The global [`ObjectSpace`]
    #[builder(setter(into))]
    pub object_space: Arc<ObjectSpace>,

    /// The [`GcBank`](crate::interpreter::gc::gc_bank::GcBank) that stores all of the upvalues in
    /// the system, from the [`Vm`](crate::interpreter::vm::Vm).
    #[builder(setter(into))]
    pub vm_upvalues: Arc<RwLock<GcRefBank>>,

    /// Call out handling, passed down from the [`Vm`](crate::interpreter::vm::Vm).
    #[builder(setter(into))]
    pub call_outs: Arc<RwLock<CallOuts>>,

    /// The tx channel to send messages back to the [`Vm`](crate::interpreter::vm::Vm).
    pub tx: Sender<VmOp>,

    /// A pointer to a memory pool to allocate new values from
    #[builder(default, setter(into))]
    pub memory: Arc<Heap>,
}