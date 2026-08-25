use std::sync::Arc;

use arc_swap::ArcSwapAny;
use thin_vec::ThinVec;

use crate::interpreter::stm::VarId;
use crate::{
    interpreter::{
        process::Process,
        stm::TxnHandle,
        task_context::{TaskContext, TaskResult},
        vm::global_state::GlobalState,
    },
    util::get_simul_efuns,
};

/// A struct to handle the Task state, so we can prepare it ahead of time.
#[derive(Debug)]
pub struct TaskTemplate {
    /// The [`GlobalState`] from the [`Vm`](crate::interpreter::vm::Vm).
    pub global_state: Arc<GlobalState>,

    /// The command giver, if there was one. This might be an NPC, or None, in the case of a
    /// call_out or input_to callback.
    pub this_player: ArcSwapAny<Option<Arc<Process>>>,

    /// The upvalue_ptrs to populate the initial frame with, if any.
    pub upvalue_ptrs: Option<ThinVec<VarId>>,

    /// The transaction the task made from this template runs in. A template
    /// derived from a live task adopts that task's in-flight handle, so the
    /// spawned task joins it; a fresh top-level template (a `Vm`, a `GlobalState`)
    /// carries the empty handle, so the spawned task opens its own attempt.
    pub(crate) txn: TxnHandle,
}

impl TaskTemplate {
    /// Set the player to be used for this [`TaskTemplate`]. We often don't know
    /// who this until just before we start the task. This is just here for
    /// convenience.
    pub fn set_this_player(&self, this_player: Option<Arc<Process>>) {
        self.this_player.store(this_player);
    }

    /// Create the [`TaskContext`] for a task running in `process`.
    pub fn into_task_context(self, process: Arc<Process>) -> TaskContext {
        let simul_efuns =
            get_simul_efuns(&self.global_state.config, &self.global_state.object_space);

        TaskContext {
            global_state: self.global_state,
            process,
            result: TaskResult::new(),
            simul_efuns,
            this_player: self.this_player,
            upvalue_ptrs: self.upvalue_ptrs,
            chain_count: 0,
            // A template from a live task carries its in-flight attempt, so
            // this context joins it.
            txn: self.txn,
            command: Arc::default(),
        }
    }
}

impl Clone for TaskTemplate {
    fn clone(&self) -> Self {
        Self {
            global_state: self.global_state.clone(),
            this_player: ArcSwapAny::from(self.this_player.load_full()),
            upvalue_ptrs: self.upvalue_ptrs.clone(),
            txn: self.txn.clone(),
        }
    }
}

impl<T> From<T> for TaskTemplate
where
    T: Into<Arc<GlobalState>>,
{
    fn from(value: T) -> Self {
        Self {
            global_state: value.into(),
            this_player: ArcSwapAny::from(None),
            upvalue_ptrs: None,
            txn: TxnHandle::default(),
        }
    }
}
