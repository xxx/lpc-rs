use std::{path::PathBuf, sync::Arc};

use delegate::delegate;
use derive_builder::Builder;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;
use once_cell::sync::OnceCell;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;

use crate::{
    interpreter::{
        call_outs::CallOuts, gc::gc_bank::GcRefBank, instruction_counter::InstructionCounter,
        lpc_ref::LpcRef, memory::Memory, object_space::ObjectSpace, process::Process,
        program::Program, vm::vm_op::VmOp,
    },
    util::get_simul_efuns,
};

/// A struct to carry context during the evaluation of a single [`Task`](crate::interpreter::task::Task).
#[derive(Debug, Clone, Builder)]
pub struct TaskContext {
    /// The [`Config`] that's in use for the
    /// [`Task`](crate::interpreter::task::Task).
    #[builder(setter(into))]
    pub config: Arc<Config>,

    /// The [`Process`] that owns the function being
    /// called in this [`Task`](crate::interpreter::task::Task).
    #[builder(setter(into))]
    pub process: Arc<RwLock<Process>>,

    /// The global [`ObjectSpace`]
    #[builder(setter(into))]
    pub object_space: Arc<RwLock<ObjectSpace>>,

    /// Direct pointer to the simul efuns
    #[builder(default, setter(strip_option))]
    pub simul_efuns: Option<Arc<RwLock<Process>>>,

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
    pub memory: Arc<Memory>,

    /// A counter, to ensure that too-long-evaluations do not occur
    #[builder(default)]
    pub instruction_counter: InstructionCounter,

    /// The final result of the original function that was called
    #[builder(default)]
    pub result: OnceCell<LpcRef>,
}

impl TaskContext {
    delegate! {
        to self.instruction_counter {
            /// Increment the current task instruction count, checking for too-long-evaluations
            #[call(increment)]
            pub fn increment_instruction_count(&self, amount: usize) -> Result<usize>;

            /// Set the current task instruction count, checking for too-long-evaluations
            #[call(set)]
            pub fn set_instruction_count(&self, amount: usize) -> Result<usize>;

            /// Get the current instruction count
            #[call(count)]
            pub fn instruction_count(&self) -> usize;
        }
    }

    /// Create a new [`TaskContext`]
    #[allow(clippy::too_many_arguments)]
    pub fn new<C, P, O, M, U, A>(
        config: C,
        process: P,
        object_space: O,
        memory: M,
        vm_upvalues: U,
        call_outs: A,
        tx: Sender<VmOp>,
    ) -> Self
    where
        C: Into<Arc<Config>>,
        P: Into<Arc<RwLock<Process>>>,
        O: Into<Arc<RwLock<ObjectSpace>>>,
        M: Into<Arc<Memory>>,
        U: Into<Arc<RwLock<GcRefBank>>>,
        A: Into<Arc<RwLock<CallOuts>>>,
    {
        let config = config.into();
        let object_space = object_space.into();
        let instruction_counter = InstructionCounter::new_from_config(&config);
        let simul_efuns = {
            let space = object_space.read();
            get_simul_efuns(&config, &space)
        };

        Self {
            config,
            process: process.into(),
            object_space,
            memory: memory.into(),
            instruction_counter,
            result: OnceCell::new(),
            simul_efuns,
            vm_upvalues: vm_upvalues.into(),
            call_outs: call_outs.into(),
            tx,
        }
    }

    /// Set the process for an existing TaskContext
    pub fn with_process<P>(mut self, process: P) -> Self
    where
        P: Into<Arc<RwLock<Process>>>,
    {
        self.process = process.into();

        self
    }

    /// Lookup the process with the passed path.
    #[inline]
    pub fn lookup_process<T>(&self, path: T) -> Option<Arc<RwLock<Process>>>
    where
        T: AsRef<str>,
    {
        self.object_space.read().lookup(path).cloned()
    }

    /// Directly insert the passed [`Process`] into the object space, with
    /// in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P)
    where
        P: Into<Arc<RwLock<Process>>>,
    {
        ObjectSpace::insert_process(&self.object_space, process)
    }

    /// Remove the passed [`Process`] from the object space.
    #[inline]
    pub fn remove_process<P>(&self, process: P)
    where
        P: Into<Arc<RwLock<Process>>>,
    {
        ObjectSpace::remove_process(&self.object_space, process)
    }

    /// Convert the passed [`Program`] into a [`Process`], set its clone ID,
    /// then insert it into the object space.
    #[inline]
    pub fn insert_clone(&self, program: Arc<Program>) -> Arc<RwLock<Process>> {
        ObjectSpace::insert_clone(&self.object_space, program)
    }

    /// Get the in-game directory of the current process.
    /// This assumes an already-dedotted path
    pub fn in_game_cwd(&self) -> PathBuf {
        let process = self.process.read();
        let current_cwd = process.cwd();

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

    /// Return the [`Config`] used for the task
    #[inline]
    pub fn config(&self) -> Arc<Config> {
        self.config.clone()
    }

    /// Return the current pointer to the simul_efuns, if any
    #[inline]
    pub fn simul_efuns(&self) -> Option<Arc<RwLock<Process>>> {
        self.simul_efuns.clone()
    }

    /// Return the [`Process`] that the task roots from.
    /// This *does not* change over the life of the task.
    #[inline]
    pub fn process(&self) -> Arc<RwLock<Process>> {
        self.process.clone()
    }

    /// Return the [`ObjectSpace`]
    #[inline]
    pub fn object_space(&self) -> &Arc<RwLock<ObjectSpace>> {
        &self.object_space
    }

    /// Return the [`Memory`]
    #[inline]
    pub fn memory(&self) -> &Arc<Memory> {
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

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;
    use tokio::sync::mpsc;

    use super::*;
    use crate::interpreter::{gc::gc_bank::GcBank, program::ProgramBuilder};

    #[test]
    fn test_in_game_cwd() {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();
        let space = RwLock::new(ObjectSpace::default());
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_server("./tests/fixtures/code/foo/bar/baz.c"))
            .build()
            .unwrap();
        let process = Process::new(program);
        let upvalues = RwLock::new(GcBank::default());
        let (tx, _rx) = mpsc::channel(100);
        let call_outs = RwLock::new(CallOuts::new(tx.clone()));
        let context = TaskContext::new(
            config,
            RwLock::new(process),
            space,
            Memory::default(),
            upvalues,
            call_outs,
            tx,
        );

        assert_eq!(context.in_game_cwd().to_str().unwrap(), "/foo/bar");
    }
}
