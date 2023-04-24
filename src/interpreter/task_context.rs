use std::{path::PathBuf, sync::Arc};

use arc_swap::ArcSwapAny;
use derive_builder::Builder;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;
use once_cell::sync::OnceCell;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;

use crate::{
    interpreter::{
        call_outs::CallOuts,
        gc::gc_bank::GcRefBank,
        heap::Heap,
        lpc_ref::LpcRef,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{into_task_context::IntoTaskContext, task_template::TaskTemplate},
        vm::vm_op::VmOp,
    },
    util::get_simul_efuns,
};

/// A struct to carry context during the evaluation of a single [`Task`](crate::interpreter::task::Task).
#[derive(Debug, Builder)]
#[builder(pattern = "owned")]
pub struct TaskContext {
    /// The [`Config`] that's in use for the
    /// [`Task`](crate::interpreter::task::Task).
    #[builder(setter(into))]
    pub config: Arc<Config>,

    /// The [`Process`] that owns the function being
    /// called in this [`Task`](crate::interpreter::task::Task).
    #[builder(setter(into))]
    pub process: Arc<Process>,

    /// The global [`ObjectSpace`]
    #[builder(default, setter(into))]
    pub object_space: Arc<ObjectSpace>,

    /// Direct pointer to the simul efuns
    #[builder(default, setter(strip_option))]
    pub simul_efuns: Option<Arc<Process>>,

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

    /// The final result of the original function that was called
    #[builder(default)]
    pub result: OnceCell<LpcRef>,

    /// The command giver, if there was one. This might be an NPC, or None.
    #[builder(default, setter(strip_option))]
    pub this_player: ArcSwapAny<Option<Arc<Process>>>,
}

impl TaskContext {
    /// Create a new [`TaskContext`]
    #[allow(clippy::too_many_arguments)]
    pub fn new<C, P, O, M, U, A>(
        config: C,
        process: P,
        object_space: O,
        memory: M,
        vm_upvalues: U,
        call_outs: A,
        this_player: Option<Arc<Process>>,
        tx: Sender<VmOp>,
    ) -> Self
    where
        C: Into<Arc<Config>>,
        P: Into<Arc<Process>>,
        O: Into<Arc<ObjectSpace>>,
        M: Into<Arc<Heap>>,
        U: Into<Arc<RwLock<GcRefBank>>>,
        A: Into<Arc<RwLock<CallOuts>>>,
    {
        let config = config.into();
        let object_space = object_space.into();
        let simul_efuns = get_simul_efuns(&config, &object_space);

        Self {
            config,
            process: process.into(),
            object_space,
            memory: memory.into(),
            result: OnceCell::new(),
            simul_efuns,
            vm_upvalues: vm_upvalues.into(),
            call_outs: call_outs.into(),
            this_player: ArcSwapAny::from(this_player),
            tx,
        }
    }

    /// Create a new [`TaskContext`] from the passed [`TaskTemplate`]
    pub fn from_template(template: TaskTemplate, process: Arc<Process>) -> Self {
        let simul_efuns = get_simul_efuns(&template.config, &template.object_space);

        Self {
            config: template.config,
            process,
            object_space: template.object_space,
            memory: template.memory,
            result: OnceCell::new(),
            simul_efuns,
            vm_upvalues: template.vm_upvalues,
            call_outs: template.call_outs,
            this_player: template.this_player,
            tx: template.tx,
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
        self.object_space.lookup(path).map(|p| p.clone())
    }

    /// Directly insert the passed [`Process`] into the object space, with
    /// in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        ObjectSpace::insert_process(&self.object_space, process)
    }

    /// Remove the passed [`Process`] from the object space.
    #[inline]
    pub fn remove_process<P>(&self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        ObjectSpace::remove_process(&self.object_space, process)
    }

    /// Convert the passed [`Program`] into a [`Process`], set its clone ID,
    /// then insert it into the object space.
    #[inline]
    pub fn insert_clone(&self, program: Arc<Program>) -> Arc<Process> {
        ObjectSpace::insert_clone(&self.object_space, program)
    }

    /// Get the in-game directory of the current process.
    /// This assumes an already-dedotted path
    pub fn in_game_cwd(&self) -> PathBuf {
        let current_cwd = self.process.cwd();

        match current_cwd.strip_prefix(&*self.config.lib_dir) {
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
            .map_err(|_| LpcError::new_bug("TaskContext::set_result result already set"))
    }

    /// Consume this context, and return its `result` field.
    #[inline]
    pub fn into_result(self) -> Option<LpcRef> {
        self.result.into_inner()
    }

    // TODO: don't clone this on every call.
    /// Return the [`Config`] used for the task
    #[inline]
    pub fn config(&self) -> &Arc<Config> {
        &self.config
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
        &self.object_space
    }

    /// Return the [`Heap`]
    #[inline]
    pub fn memory(&self) -> &Arc<Heap> {
        &self.memory
    }

    /// Return the `upvalues`
    #[inline]
    pub fn upvalues(&self) -> &Arc<RwLock<GcRefBank>> {
        &self.vm_upvalues
    }

    /// Get the final result of the Task that this context is for, if it's finished.
    #[inline]
    pub fn result(&self) -> Option<&LpcRef> {
        self.result.get()
    }

    /// Get the [`CallOuts`] for this task
    #[inline]
    pub fn call_outs(&self) -> &Arc<RwLock<CallOuts>> {
        &self.call_outs
    }

    /// Get the `tx` channel for this task
    #[inline]
    pub fn tx(&self) -> Sender<VmOp> {
        self.tx.clone()
    }
}

impl Clone for TaskContext {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            process: self.process.clone(),
            object_space: self.object_space.clone(),
            memory: self.memory.clone(),
            result: OnceCell::new(),
            simul_efuns: self.simul_efuns.clone(),
            vm_upvalues: self.vm_upvalues.clone(),
            call_outs: self.call_outs.clone(),
            this_player: ArcSwapAny::from(self.this_player.load_full()),
            tx: self.tx.clone(),
        }
    }
}

impl IntoTaskContext for TaskContext {
    fn into_task_context(self, _proc: Arc<Process>) -> TaskContext {
        self
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;
    use tokio::sync::mpsc;

    use super::*;
    use crate::interpreter::program::ProgramBuilder;

    #[test]
    fn test_in_game_cwd() {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();
        let space = ObjectSpace::default();
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_server("./tests/fixtures/code/foo/bar/baz.c"))
            .build()
            .unwrap();
        let process = Process::new(program);
        let (tx, _rx) = mpsc::channel(100);
        let call_outs = RwLock::new(CallOuts::new(tx.clone()));
        let context = TaskContextBuilder::default()
            .config(config)
            .process(process)
            .object_space(space)
            .call_outs(call_outs)
            .tx(tx)
            .build()
            .unwrap();

        assert_eq!(context.in_game_cwd().to_str().unwrap(), "/foo/bar");
    }
}
