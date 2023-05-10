use std::{future::Future, path::PathBuf, sync::Arc};

use arc_swap::ArcSwapAny;
use async_trait::async_trait;
use derive_builder::Builder;
use lpc_rs_core::register::Register;
use lpc_rs_errors::{lpc_bug, Result};
use lpc_rs_utils::config::Config;
use once_cell::sync::OnceCell;
use parking_lot::RwLock;
use thin_vec::ThinVec;
use tokio::sync::mpsc::Sender;

use crate::{
    compiler::Compiler,
    interpreter::{
        call_outs::CallOuts,
        gc::gc_bank::GcRefBank,
        heap::Heap,
        lpc_ref::LpcRef,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{into_task_context::IntoTaskContext, task_template::TaskTemplate},
        vm::{global_state::GlobalState, vm_op::VmOp},
    },
    util::{
        get_simul_efuns,
        process_builder::{ProcessCreator, ProcessInitializer},
        with_compiler::WithCompiler,
    },
};

/// A struct to carry context during the evaluation of a single [`Task`](crate::interpreter::task::Task).
#[derive(Debug, Builder)]
#[builder(pattern = "owned")]
pub struct TaskContext {
    /// The [`GlobalState`] from the [`Vm`](crate::interpreter::vm::Vm).
    #[builder(setter(into))]
    pub global_state: Arc<GlobalState>,

    /// The [`Process`] that owns the function being
    /// called in this [`Task`](crate::interpreter::task::Task).
    // TODO: this is not accurate in the case of some call_others, or function ptr calls,
    //       The process in the call frame is more accurate, and this probably should be removed.
    #[builder(setter(into))]
    pub process: Arc<Process>,

    /// Direct pointer to the simul efuns
    #[builder(default, setter(strip_option))]
    pub simul_efuns: Option<Arc<Process>>,

    /// The final result of the original function that was called
    #[builder(default)]
    pub result: OnceCell<LpcRef>,

    /// The command giver, if there was one. This might be an NPC, or None.
    // TODO: put this into an Arc so it can shared more easily between multiple contexts.
    //       also make this Weak
    #[builder(default, setter(strip_option))]
    pub this_player: ArcSwapAny<Option<Arc<Process>>>,

    /// The upvalue_ptrs to populate the initial frame with, if any.
    #[builder(default)]
    pub upvalue_ptrs: Option<ThinVec<Register>>,

    /// The number of this task in the current chain of Tasks. This is
    /// used to prevent infinite recursion among multiple Tasks.
    #[builder(default)]
    pub chain_count: u8,
}

impl TaskContext {
    /// Create a new [`TaskContext`]
    pub fn new<P>(
        global_state: Arc<GlobalState>,
        process: P,
        this_player: Option<Arc<Process>>,
        upvalue_ptrs: Option<ThinVec<Register>>,
    ) -> Self
    where
        P: Into<Arc<Process>>,
    {
        let simul_efuns = get_simul_efuns(&global_state.config, &global_state.object_space);

        Self {
            global_state,
            process: process.into(),
            result: OnceCell::new(),
            simul_efuns,
            this_player: ArcSwapAny::from(this_player),
            upvalue_ptrs,
            chain_count: 0,
        }
    }

    /// Create a new [`TaskContext`] from the passed [`TaskTemplate`]
    pub fn from_template(template: TaskTemplate, process: Arc<Process>) -> Self {
        let simul_efuns = get_simul_efuns(
            &template.global_state.config,
            &template.global_state.object_space,
        );

        Self {
            global_state: template.global_state,
            process,
            result: OnceCell::new(),
            simul_efuns,
            this_player: template.this_player,
            upvalue_ptrs: template.upvalue_ptrs,
            chain_count: 0,
        }
    }

    /// Set the process for an existing TaskContext
    pub fn with_process<P>(mut self, process: P) -> Self
    where
        P: Into<Arc<Process>>,
    {
        self.process = process.into();

        self
    }

    /// Lookup the process with the passed path.
    #[inline]
    pub fn lookup_process<T>(&self, path: T) -> Option<Arc<Process>>
    where
        T: AsRef<str>,
    {
        self.object_space().lookup(path).map(|p| p.clone())
    }

    /// Directly insert the passed [`Process`] into the object space, with
    /// in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        ObjectSpace::insert_process(self.object_space(), process)
    }

    /// Remove the passed [`Process`] from the object space.
    #[inline]
    pub fn remove_process<P>(&self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        ObjectSpace::remove_process(self.object_space(), process)
    }

    /// Convert the passed [`Program`] into a [`Process`], set its clone ID,
    /// then insert it into the object space.
    #[inline]
    pub fn insert_clone(&self, program: Arc<Program>) -> Arc<Process> {
        ObjectSpace::insert_clone(self.object_space(), program)
    }

    /// Get the in-game directory of the current process.
    /// This assumes an already-dedotted path
    pub fn in_game_cwd(&self) -> PathBuf {
        let current_cwd = self.process.cwd();

        match current_cwd.strip_prefix(&*self.config().lib_dir) {
            Ok(x) => {
                if x.as_os_str().is_empty() {
                    PathBuf::from("/")
                } else if x.starts_with("/") {
                    x.to_path_buf()
                } else {
                    PathBuf::from(format!("/{}", x.display()))
                }
            }
            Err(_e) => current_cwd.into_owned(),
        }
    }

    /// Update the context's `result` with the passed [`LpcRef`]
    #[inline]
    pub fn set_result(&self, new_result: LpcRef) -> Result<()> {
        self.result
            .set(new_result)
            .map_err(|_| lpc_bug!("TaskContext::set_result result already set"))
    }

    /// Consume this context, and return its `result` field.
    #[inline]
    pub fn into_result(self) -> Option<LpcRef> {
        self.result.into_inner()
    }

    /// Return the [`Config`] used for the task
    #[inline]
    pub fn config(&self) -> &Arc<Config> {
        &self.global_state.config
    }

    /// Return the current pointer to the simul_efuns, if any
    #[inline]
    pub fn simul_efuns(&self) -> Option<&Arc<Process>> {
        self.simul_efuns.as_ref()
    }

    /// Return the [`Process`] that the task roots from.
    /// This *does not* change over the life of the task.
    #[inline]
    pub fn process(&self) -> &Arc<Process> {
        &self.process
    }

    /// Return the [`ObjectSpace`]
    #[inline]
    pub fn object_space(&self) -> &Arc<ObjectSpace> {
        &self.global_state.object_space
    }

    /// Return the [`Heap`]
    #[inline]
    pub fn memory(&self) -> &Heap {
        &self.global_state.memory
    }

    /// Return the `upvalues`
    #[inline]
    pub fn upvalues(&self) -> &Arc<RwLock<GcRefBank>> {
        &self.global_state.upvalues
    }

    /// Get the final result of the Task that this context is for, if it's finished.
    #[inline]
    pub fn result(&self) -> Option<&LpcRef> {
        self.result.get()
    }

    /// Get the [`CallOuts`] for this task
    #[inline]
    pub fn call_outs(&self) -> &Arc<RwLock<CallOuts>> {
        &self.global_state.call_outs
    }

    /// Get the `tx` channel for this task
    #[inline]
    pub fn tx(&self) -> Sender<VmOp> {
        self.global_state.tx.clone()
    }
}

impl Clone for TaskContext {
    fn clone(&self) -> Self {
        Self {
            global_state: self.global_state.clone(),
            process: self.process.clone(),
            result: OnceCell::new(),
            simul_efuns: self.simul_efuns.clone(),
            this_player: ArcSwapAny::from(self.this_player.load_full()),
            upvalue_ptrs: self.upvalue_ptrs.clone(),
            chain_count: self.chain_count,
        }
    }
}

impl AsRef<Heap> for TaskContext {
    fn as_ref(&self) -> &Heap {
        &self.global_state.memory
    }
}

impl AsRef<ObjectSpace> for TaskContext {
    fn as_ref(&self) -> &ObjectSpace {
        &self.global_state.object_space
    }
}

impl IntoTaskContext for TaskContext {
    fn into_task_context(self, _proc: Arc<Process>) -> TaskContext {
        self
    }
}

#[async_trait]
impl WithCompiler for TaskContext {
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

#[async_trait]
impl ProcessCreator for TaskContext {
    fn process_creator_data(&self) -> &ObjectSpace {
        &self.global_state.object_space
    }
}

#[async_trait]
impl ProcessInitializer for TaskContext {
    fn process_initializer_data(&self) -> TaskTemplate {
        TaskTemplate::from(self)
    }
}

impl From<TaskContext> for TaskContextBuilder {
    fn from(value: TaskContext) -> Self {
        Self {
            global_state: Some(value.global_state),
            process: Some(value.process),
            simul_efuns: Some(value.simul_efuns),
            result: None,
            this_player: Some(value.this_player),
            upvalue_ptrs: Some(value.upvalue_ptrs),
            chain_count: Some(value.chain_count),
        }
    }
}

impl From<&TaskContext> for TaskContextBuilder {
    fn from(value: &TaskContext) -> Self {
        Self {
            global_state: Some(value.global_state.clone()),
            process: Some(value.process.clone()),
            simul_efuns: Some(value.simul_efuns.clone()),
            result: None,
            this_player: Some(ArcSwapAny::from(value.this_player.load_full())),
            upvalue_ptrs: Some(value.upvalue_ptrs.clone()),
            chain_count: Some(value.chain_count),
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use tokio::sync::mpsc;

    use super::*;
    use crate::{interpreter::program::ProgramBuilder, test_support::test_config};

    #[test]
    fn test_in_game_cwd() {
        // let config = ConfigBuilder::default()
        //     .lib_dir("./tests/fixtures/code/")
        //     .build()
        //     .unwrap();
        let config = test_config();
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_server("./tests/fixtures/code/foo/bar/baz.c"))
            .build()
            .unwrap();
        let process = Process::new(program);
        let (tx, _rx) = mpsc::channel(100);
        let global_state = GlobalState::new(config, tx);
        let context = TaskContextBuilder::default()
            .global_state(global_state)
            .process(process)
            .build()
            .unwrap();

        assert_eq!(context.in_game_cwd().to_str().unwrap(), "/foo/bar");
    }
}
