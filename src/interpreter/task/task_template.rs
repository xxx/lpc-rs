use std::sync::Arc;

use arc_swap::ArcSwapAny;
use derive_builder::Builder;
use lpc_rs_core::register::Register;
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;

use crate::interpreter::{
    call_outs::CallOuts, gc::gc_bank::GcRefBank, heap::Heap, object_space::ObjectSpace,
    process::Process, task::into_task_context::IntoTaskContext, task_context::TaskContext,
    vm::vm_op::VmOp,
};

/// A struct to handle the non-changing Task state, so we can prepare it ahead of time.
#[derive(Debug, Builder)]
#[builder(pattern = "owned")]
pub struct TaskTemplate {
    /// The [`Config`] that's in use for the
    /// [`Task`](crate::interpreter::task::Task).
    #[builder(setter(into))]
    pub config: Arc<Config>,

    /// The global [`ObjectSpace`]
    #[builder(default, setter(into))]
    pub object_space: Arc<ObjectSpace>,

    /// The [`GcBank`](crate::interpreter::gc::gc_bank::GcBank) that stores all of the upvalues in
    /// the system, from the [`Vm`](crate::interpreter::vm::Vm).
    #[builder(default, setter(into))]
    pub vm_upvalues: Arc<RwLock<GcRefBank>>,

    /// Call out handling, passed down from the [`Vm`](crate::interpreter::vm::Vm).
    #[builder(setter(into))]
    pub call_outs: Arc<RwLock<CallOuts>>,

    /// The tx channel to send messages back to the [`Vm`](crate::interpreter::vm::Vm).
    pub tx: Sender<VmOp>,

    /// A pointer to a memory pool to allocate new values from
    #[builder(default, setter(into))]
    pub memory: Arc<Heap>,

    /// The command giver, if there was one. This might be an NPC, or None, in the case of a
    /// call_out or input_to callback.
    #[builder(default, setter(into))]
    pub this_player: ArcSwapAny<Option<Arc<Process>>>,

    /// The upvalue_ptrs to populate the initial frame with, if any.
    #[builder(default)]
    pub upvalue_ptrs: Option<Vec<Register>>,
}

impl TaskTemplate {
    /// Set the player to be used for this [`TaskTemplate`]. We often don't know
    /// who this until just before we start the task. This is just here for
    /// convenience.
    pub fn set_this_player(&self, this_player: Option<Arc<Process>>) {
        self.this_player.store(this_player);
    }
}

impl Clone for TaskTemplate {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            object_space: self.object_space.clone(),
            vm_upvalues: self.vm_upvalues.clone(),
            call_outs: self.call_outs.clone(),
            tx: self.tx.clone(),
            memory: self.memory.clone(),
            this_player: ArcSwapAny::from(self.this_player.load_full()),
            upvalue_ptrs: self.upvalue_ptrs.clone(),
        }
    }
}

impl IntoTaskContext for TaskTemplate {
    fn into_task_context(self, process: Arc<Process>) -> TaskContext {
        TaskContext::from_template(self, process)
    }
}
