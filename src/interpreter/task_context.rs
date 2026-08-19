use std::{future::Future, path::PathBuf, sync::Arc};

use arc_swap::ArcSwapAny;
use async_trait::async_trait;
use derive_builder::Builder;
use lpc_rs_core::register::Register;
use lpc_rs_errors::{Result, lpc_bug};
use lpc_rs_utils::config::Config;
use thin_vec::ThinVec;
use tokio::sync::mpsc::Sender;

use crate::{
    compiler::Compiler,
    interpreter::{
        call_outs::CallOuts,
        lpc_ref::LpcRef,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        stm::TxnHandle,
        task::{into_task_context::IntoTaskContext, task_template::TaskTemplate},
        vm::{global_state::GlobalState, vm_op::VmOp},
    },
    util::{
        get_simul_efuns,
        process_builder::{ProcessCreator, ProcessInitializer},
        with_compiler::WithCompiler,
    },
};

/// The task's final result, resettable between retry attempts.
#[derive(Debug)]
pub struct TaskResult {
    inner: std::sync::Mutex<Option<LpcRef>>,
}

impl TaskResult {
    pub fn new() -> Self {
        Self {
            inner: std::sync::Mutex::new(None),
        }
    }

    /// Set the result.
    pub fn set(&self, new_result: LpcRef) -> Result<()> {
        let mut g = self.inner.lock().unwrap();
        if g.is_some() {
            return Err(lpc_bug!("TaskResult::set result already set"));
        }
        *g = Some(new_result);
        Ok(())
    }

    /// Return a clone of the stored result. `LpcRef` is cheap to clone
    /// (small ints, or `Arc`/`Weak` payloads) and avoids borrow-checker handling
    /// on the hot path.
    pub fn get(&self) -> Option<LpcRef> {
        self.inner.lock().unwrap().clone()
    }

    pub fn into_inner(self) -> Option<LpcRef> {
        self.inner.into_inner().unwrap()
    }

    /// Clear the result (for a retry re-run).
    pub fn reset(&self) {
        *self.inner.lock().unwrap() = None;
    }
}

impl Default for TaskResult {
    fn default() -> Self {
        Self::new()
    }
}

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

    /// The final result of the original function that was called.
    #[builder(default)]
    pub result: TaskResult,

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

    /// This task's transaction handle. Top-level tasks re-base it per
    /// attempt; sub-tasks adopt their caller's handle.
    #[builder(default)]
    pub(crate) txn: TxnHandle,
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
            result: TaskResult::new(),
            simul_efuns,
            this_player: ArcSwapAny::from(this_player),
            upvalue_ptrs,
            chain_count: 0,
            txn: TxnHandle::default(),
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
            result: TaskResult::new(),
            simul_efuns,
            this_player: template.this_player,
            upvalue_ptrs: template.upvalue_ptrs,
            chain_count: 0,
            // The template's handle is carried through as-is: a template
            // derived from a live task holds that task's in-flight attempt,
            // so this context joins it; a fresh top-level
            // template holds the empty handle, so this one opens its own.
            txn: template.txn,
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
        self.object_space().lookup(path)
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
        self.result.set(new_result)
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

    /// The transaction this task runs in.
    #[inline]
    pub(crate) fn txn(&self) -> &TxnHandle {
        &self.txn
    }

    /// Return the [`ObjectSpace`]
    #[inline]
    pub fn object_space(&self) -> &Arc<ObjectSpace> {
        &self.global_state.object_space
    }

    /// Get the final result of the Task that this context is for, if it's
    /// finished. Returns an owned clone (see [`TaskResult::get`]).
    #[inline]
    pub fn result(&self) -> Option<LpcRef> {
        self.result.get()
    }

    /// Clear the result so a retry re-run can record a fresh one.
    #[inline]
    pub fn reset(&self) {
        self.result.reset()
    }

    /// Get the `tx` channel for this task
    #[inline]
    pub fn tx(&self) -> Sender<VmOp> {
        self.global_state.tx.clone()
    }

    /// Access call-outs
    pub fn with_call_outs<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&CallOuts) -> R,
    {
        self.global_state.with_call_outs(f)
    }

    /// Access call-outs mutably
    pub fn with_call_outs_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut CallOuts) -> R,
    {
        self.global_state.with_call_outs_mut(f)
    }
}

impl Clone for TaskContext {
    fn clone(&self) -> Self {
        Self {
            global_state: self.global_state.clone(),
            process: self.process.clone(),
            result: TaskResult::new(),
            simul_efuns: self.simul_efuns.clone(),
            this_player: ArcSwapAny::from(self.this_player.load_full()),
            upvalue_ptrs: self.upvalue_ptrs.clone(),
            chain_count: self.chain_count,
            // Cloning the handle shares the in-flight transaction.
            txn: self.txn.clone(),
        }
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
            txn: Some(value.txn),
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
            txn: Some(value.txn.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use tokio::sync::mpsc;

    use super::*;
    use crate::{
        interpreter::{lpc_ref::LpcRef, program::ProgramBuilder},
        test_support::test_config,
    };

    fn result_test_ctx() -> TaskContext {
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_server("./tests/fixtures/code/foo/bar/baz.c"))
            .build()
            .unwrap();
        let process = Process::new(program);
        let (tx, _rx) = mpsc::channel(100);
        let global_state = GlobalState::new(test_config(), tx);
        TaskContextBuilder::default()
            .global_state(global_state)
            .process(process)
            .build()
            .unwrap()
    }

    #[test]
    fn result_set_get_reset_cycle() {
        let c = result_test_ctx();
        assert!(c.result().is_none());
        c.set_result(LpcRef::from(7i64)).unwrap();
        assert_eq!(c.result(), Some(LpcRef::from(7i64)));
        c.reset();
        assert!(c.result().is_none());
        c.set_result(LpcRef::from(8i64)).unwrap();
        assert_eq!(c.result(), Some(LpcRef::from(8i64)));
    }

    #[test]
    #[should_panic(expected = "already set")]
    fn result_double_set_is_a_bug() {
        let c = result_test_ctx();
        c.set_result(LpcRef::from(1i64)).unwrap();
        c.set_result(LpcRef::from(2i64)).unwrap();
    }

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
