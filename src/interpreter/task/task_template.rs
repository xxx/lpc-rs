use std::{future::Future, sync::Arc};

use arc_swap::ArcSwapAny;
use async_trait::async_trait;
use derive_builder::Builder;
use lpc_rs_core::register::Register;
use lpc_rs_errors::Result;
use thin_vec::ThinVec;

use crate::{
    compiler::Compiler,
    interpreter::{
        heap::Heap, object_space::ObjectSpace, process::Process,
        task::into_task_context::IntoTaskContext, task_context::TaskContext,
        vm::global_state::GlobalState,
    },
    util::with_compiler::WithCompiler,
};

/// A struct to handle the Task state, so we can prepare it ahead of time.
#[derive(Debug, Builder)]
#[builder(pattern = "owned")]
pub struct TaskTemplate {
    /// The [`GlobalState`] from the [`Vm`](crate::interpreter::vm::Vm).
    #[builder(setter(into))]
    pub global_state: Arc<GlobalState>,

    /// The command giver, if there was one. This might be an NPC, or None, in the case of a
    /// call_out or input_to callback.
    #[builder(default, setter(into))]
    pub this_player: ArcSwapAny<Option<Arc<Process>>>,

    /// The upvalue_ptrs to populate the initial frame with, if any.
    #[builder(default)]
    pub upvalue_ptrs: Option<ThinVec<Register>>,
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
            global_state: self.global_state.clone(),
            this_player: ArcSwapAny::from(self.this_player.load_full()),
            upvalue_ptrs: self.upvalue_ptrs.clone(),
        }
    }
}

impl AsRef<Heap> for TaskTemplate {
    fn as_ref(&self) -> &Heap {
        &self.global_state.memory
    }
}

impl AsRef<ObjectSpace> for TaskTemplate {
    fn as_ref(&self) -> &ObjectSpace {
        &self.global_state.object_space
    }
}

impl IntoTaskContext for TaskTemplate {
    fn into_task_context(self, process: Arc<Process>) -> TaskContext {
        TaskContext::from_template(self, process)
    }
}

impl From<TaskContext> for TaskTemplate {
    fn from(task_context: TaskContext) -> Self {
        Self {
            global_state: task_context.global_state,
            this_player: task_context.this_player,
            upvalue_ptrs: task_context.upvalue_ptrs,
        }
    }
}

impl From<&TaskContext> for TaskTemplate {
    fn from(task_context: &TaskContext) -> Self {
        Self {
            global_state: task_context.global_state.clone(),
            this_player: ArcSwapAny::from(None),
            upvalue_ptrs: task_context.upvalue_ptrs.clone(),
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
        }
    }
}

#[async_trait]
impl WithCompiler for TaskTemplate {
    async fn with_async_compiler<F, U, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce(Compiler) -> U + Send,
        U: Future<Output = Result<T>> + Send,
    {
        Self::with_async_compiler_associated(
            f,
            &self.global_state.config,
            &self.global_state.object_space,
        )
        .await
    }
}
