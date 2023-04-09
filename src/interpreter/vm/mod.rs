use std::{cmp::Ordering, fmt::Formatter, hash::Hasher, path::Path, sync::Arc};

use bit_set::BitSet;
use educe::Educe;
use if_chain::if_chain;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use lpc_rs_utils::config::Config;
use qcell::{QCell, QCellOwner};
use tokio::sync::mpsc::{Receiver, Sender};
use tracing::{error, instrument, trace, warn};
use vm_op::VmOp;

use crate::{
    compile_time_config::{MAX_CALL_STACK_SIZE, VM_CHANNEL_CAPACITY},
    compiler::{Compiler, CompilerBuilder},
    interpreter::{
        call_outs::CallOuts,
        efun::EFUN_PROTOTYPES,
        function_type::function_address::FunctionAddress,
        gc::{
            gc_bank::{GcBank, GcRefBank},
            mark::Mark,
            sweep::KeylessSweep,
        },
        lpc_ref::{LpcRef, NULL},
        lpc_value::LpcValue,
        memory::Memory,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{task_state::TaskState, Task},
        task_context::TaskContext,
        vm::task_queue::TaskQueue,
    },
    try_extract_value,
    util::{get_simul_efuns, keyable::Keyable, qcell_debug},
};

pub mod task_queue;
pub mod vm_op;

#[derive(Educe)]
#[educe(Debug)]
#[readonly::make]
pub struct Vm {
    /// Our object space, which stores all of the system objects (masters and clones)
    #[educe(Debug(method = "qcell_debug"))]
    pub object_space: Arc<QCell<ObjectSpace>>,

    /// Shared VM memory. Reference-type `LpcRef`s are allocated out of this.
    memory: Arc<Memory>,

    /// All upvalues are stored in the [`Vm`], and are shared between all [`Task`]s
    #[educe(Debug(method = "qcell_debug"))]
    pub upvalues: Arc<QCell<GcRefBank>>,

    /// The [`Config`] that's in use for this [`Vm`]
    config: Arc<Config>,

    /// Enqueued call outs
    #[educe(Debug(method = "qcell_debug"))]
    call_outs: Arc<QCell<CallOuts>>,

    /// The channel used to send [`VmOp`]s to this [`Vm`]
    tx: Sender<VmOp>,

    /// The channel used to receive [`VmOp`]s from other locations
    rx: Receiver<VmOp>,

    /// The key used to create all [`QCell`]s
    #[educe(Debug(method = "qcell_debug"))]
    pub cell_key: QCellOwner,

    /// The queue for [`Task`]s.
    pub task_queue: TaskQueue,
}

impl Vm {
    /// Create a new [`Vm`].
    pub fn new<C>(config: C) -> Self
    where
        C: Into<Arc<Config>>,
    {
        Self::new_with_key(config, QCellOwner::new())
    }

    /// Create a new [`Vm`], using the specified [`QCellOwner`].
    pub fn new_with_key<C>(config: C, cell_key: QCellOwner) -> Self
    where
        C: Into<Arc<Config>>,
    {
        let object_space = ObjectSpace::default();
        let (tx, rx) = tokio::sync::mpsc::channel(VM_CHANNEL_CAPACITY);
        let call_outs = cell_key.cell(CallOuts::new(tx.clone()));
        Self {
            object_space: Arc::new(cell_key.cell(object_space)),
            memory: Arc::new(Memory::default()),
            config: config.into(),
            upvalues: Arc::new(cell_key.cell(GcBank::default())),
            call_outs: Arc::new(call_outs),
            rx,
            tx,
            task_queue: TaskQueue::new(),
            cell_key,
        }
    }

    /// The main initialization method for the VM.
    ///
    /// This method will load the master object and simul_efun file, add
    /// the master object to the object space, and then start the main loop.
    ///
    /// # Arguments
    ///
    /// * `cell_key` - The [`QCellOwner`] that will be used to create _all_ [`QCell`]s
    ///                in the system. Don't lose this key.
    pub async fn boot(&mut self) -> Result<()> {
        self.bootstrap()?;

        self.run().await?;

        Ok(())
    }

    /// Load and initialize the master object and simul_efuns.
    ///
    /// # Returns
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the master object
    /// * `Err(LpcError)` - If there was an error.
    pub fn bootstrap(&mut self) -> Result<TaskContext> {
        if let Some(Err(e)) = self.initialize_simul_efuns() {
            e.emit_diagnostics();
            return Err(e);
        }

        let master_path =
            LpcPath::new_in_game(&*self.config.master_object, "/", &*self.config.lib_dir);
        self.initialize_file(&master_path)
    }

    /// Run the [`Vm`]'s main loop.
    /// Assumes `bootstrap()` has already been called.
    /// # Arguments
    ///
    /// * `cell_key` - The [`QCellOwner`] that will be used to create _all_ [`QCell`]s
    ///                in the system.
    #[instrument(skip_all)]
    pub async fn run(&mut self) -> Result<()> {
        loop {
            // Run some code for a bit
            println!("looping");
            tokio::select! {
                Some(op) = self.rx.recv() => {
                    trace!(?op, "Received VmOp");

                    match op {
                        VmOp::PrioritizeCallOut(idx) => {
                            self.op_prioritize_call_out(idx)?;
                        } /* VmOp::Yield => {
                           *     self.op_yield_task()?;
                           * }
                           * VmOp::FinishTask(task_id) => {
                           *     self.op_finish_task()?;
                           * } */
                    }
                }

                Some(task) = async {self.task_queue.current_mut()} => {
                    trace!("resuming task {:?}", task);
                    match task.resume(&mut self.cell_key) {
                        Ok(()) => match task.state {
                            TaskState::Complete | TaskState::Error => {
                                self.task_queue.finish_current();
                            }
                            TaskState::Paused => {
                                self.task_queue.switch_to_next();
                            }
                            TaskState::New | TaskState::Running => {
                                error!(
                                    "Task {} returned from resume() in an invalid state: {}",
                                    task.id, task.state
                                );
                                self.task_queue.switch_to_next();
                            }
                        },
                        Err(e) => {
                            e.emit_diagnostics();
                            self.task_queue.finish_current();
                        }
                    }
                },

                // _ = tokio::time::sleep(std::time::Duration::from_millis(100)) => {}
            }

            // if let Some(task) = self.task_queue.current_mut() {
            //     match task.resume(&mut self.cell_key) {
            //         Ok(()) => match task.state {
            //             TaskState::Complete | TaskState::Error => {
            //                 self.task_queue.finish_current();
            //             }
            //             TaskState::Paused => {
            //                 self.task_queue.switch_to_next();
            //             }
            //             TaskState::New | TaskState::Running => {
            //                 error!(
            //                     "Task {} returned from resume() in an invalid state: {}",
            //                     task.id, task.state
            //                 );
            //                 self.task_queue.switch_to_next();
            //             }
            //         },
            //         Err(e) => {
            //             e.emit_diagnostics();
            //             self.task_queue.finish_current();
            //         }
            //     }
            // }

            // Check for bus messages
            // match self.rx.recv() {
            //     Ok(op) => {
            //         trace!(?op, "Received VmOp");
            //
            //         match op {
            //             VmOp::PrioritizeCallOut(idx) => {
            //                 self.op_prioritize_call_out(idx)?;
            //             } /* VmOp::Yield => {
            //                *     self.op_yield_task()?;
            //                * }
            //                * VmOp::FinishTask(task_id) => {
            //                *     self.op_finish_task()?;
            //                * } */
            //         }
            //     }
            //     Err(e) => {
            //         if e == RecvTimeoutError::Timeout {
            //             // No messages
            //         } else {
            //             // Something went wrong
            //             panic!("Error receiving from channel: {}", e);
            //         }
            //     }
            // }
        }
    }

    /// Handler for [`VmOp::PrioritizeCallOut`].
    ///
    /// # Arguments
    ///
    /// * `idx` - The index of the call out to run
    /// * `cell_key` - The [`QCellOwner`] that will be used to access [`QCell`]s
    ///
    /// # Returns
    ///
    /// * `Ok(true))` - If the call out was prioritized successfully
    /// * `Ok(false)` - If the call out was not found (e.g. has been removed)
    /// * `Err(LpcError)` - If there was an error prioritizing the call out
    fn op_prioritize_call_out(&mut self, idx: usize) -> Result<bool> {
        if self.call_outs.ro(&self.cell_key).get(idx).is_none() {
            return Ok(false);
        }

        let repeating: bool;

        let (process, function, args) = if_chain! {
            let call_out = self.call_outs.ro(&self.cell_key).get(idx).unwrap();
            if let LpcRef::Function(ref func) = call_out.func_ref;
            then {
                repeating = call_out.is_repeating();
                let b = func.read();
                let ptr = try_extract_value!(&*b, LpcValue::Function);

                // call outs don't get any additional args passed to them, so just set up the partial args.
                // use int 0 for any that were not applied at the time the pointer was created
                // TODO: error instead of int 0?
                let args = ptr.partial_args.iter().map(|arg| {
                    match arg {
                        Some(lpc_ref) => lpc_ref.clone(),
                        None => NULL
                    }
                }).collect::<Vec<_>>();

                match ptr.address {
                    FunctionAddress::Local(ref proc, ref function) => (proc.clone(), function.clone(), args),
                    FunctionAddress::Dynamic(_) => {
                        return Err(LpcError::new(
                            "function with dynamic receiver passed to call_out".to_string(),
                        ));
                    },
                    FunctionAddress::SimulEfun(name) => {
                        let Some(simul_efuns) = get_simul_efuns(&self.config, self.object_space.ro(&self.cell_key)) else {
                            return Err(LpcError::new_bug(
                                "function pointer to simul_efun passed, but no simul_efuns?".to_string(),
                            ));
                        };

                        let Some(function) = simul_efuns.ro(&self.cell_key).program.lookup_function(name) else {
                            return Err(LpcError::new(format!("call to unknown simul_efun `{name}`")));
                        };

                        (simul_efuns.clone(), function.clone(), args)
                    },
                    FunctionAddress::Efun(name) => {
                        let prototype = EFUN_PROTOTYPES.get(name.as_str()).unwrap();
                        // TODO: prototypes should be in Rcs so this clone is cheap
                        let pf = ProgramFunction::new(prototype.clone(), 0);

                        (Arc::new(self.cell_key.cell(Process::default())), Arc::new(pf), args)
                    }
                }
            }
            else {
                return Err(LpcError::new("invalid function sent to `call_out`"));
            }
        };

        let call_outs = self.call_outs.rw(&mut self.cell_key);
        if repeating {
            call_outs.get_mut(idx).unwrap().refresh();
        } else {
            call_outs.remove(idx);
        }

        let task_context = TaskContext::new(
            self.config.clone(),
            process.clone(),
            self.object_space.clone(),
            self.memory.clone(),
            self.upvalues.clone(),
            self.call_outs.clone(),
            self.tx.clone(),
            &self.cell_key,
        );

        let mut task = Task::<MAX_CALL_STACK_SIZE>::new(task_context);

        task.prepare_function_call(process, function, &args, &mut self.cell_key)?;

        self.task_queue.push(task);

        Ok(true)
    }

    // /// Handler for [`VmOp::Yield`].
    // /// Yield the current task, and switch to the next one in the queue.
    // #[inline]
    // pub fn op_yield_task(&mut self) -> Result<()> {
    //     self.task_queue.switch_to_next();
    //
    //     Ok(())
    // }
    //
    // /// Handler for [`VmOp::Finish`].
    // /// Finish the current task, and switch to the next one in the queue.
    // #[inline]
    // pub fn op_finish_task(&mut self) -> Result<()> {
    //     self.task_queue.finish_current();
    //
    //     Ok(())
    // }

    /// Initialize the simulated efuns file, if it is configured.
    ///
    /// # Returns
    ///
    /// * `Some(Ok(TaskContext))` - The [`TaskContext`] for the simul_efun file
    /// * `Some(Err(LpcError))` - If there was an error loading the simul_efun file
    /// * `None` - If there is no simul_efun file configured
    pub fn initialize_simul_efuns(&mut self) -> Option<Result<TaskContext>> {
        let Some(path) = &self.config.simul_efun_file else {
            return None
        };

        let simul_efun_path = LpcPath::new_in_game(path.as_str(), "/", &*self.config.lib_dir);
        Some(self.initialize_file(&simul_efun_path))
    }

    /// Do a full garbage collection cycle.
    #[instrument(skip_all)]
    pub fn gc(&mut self) -> Result<()> {
        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        self.mark(&mut marked, &mut processed, &self.cell_key)
            .unwrap();

        trace!("Marked {} objects", marked.len());

        self.keyless_sweep(&marked)
    }

    /// Compile and initialize code from the passed file.
    fn initialize_file(&mut self, filename: &LpcPath) -> Result<TaskContext> {
        debug_assert!(matches!(filename, &LpcPath::InGame(_)));

        self.with_compiler(|compiler, cell_key| {
            compiler.compile_in_game_file(filename, None, cell_key)
        })
        .and_then(|program| self.create_and_initialize_task(program))
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
    }

    /// Compile and initialize arbitrary code from the passed string.
    /// The filename is assigned as if the code were read from a real file.
    ///
    /// # Arguments
    ///
    /// * `code` - The code to compile and initialize
    /// * `filename` - The filename to assign to the code. It's assumed to be an in-game path,
    ///                with [`lib_dir`](Config) as the root.
    /// * `cell_key` - The [`QCellOwner`] that will be used to create any necessary [`QCell`]s
    ///
    /// # Returns
    ///
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the code
    /// * `Err(LpcError)` - If there was an error compiling or initializing the code
    ///
    /// # Examples
    ///
    /// ```
    /// use lpc_rs::interpreter::{lpc_ref::LpcRef, vm::Vm};
    /// use lpc_rs_utils::config::Config;
    /// use qcell::QCellOwner;
    ///
    /// let mut vm = Vm::new(Config::default());
    /// let ctx = vm.initialize_string("int x = 5;", "test.c").unwrap();
    ///
    /// assert_eq!(
    ///     ctx.process().ro(&vm.cell_key).globals.registers[0],
    ///     LpcRef::Int(5)
    /// );
    /// assert!(vm.object_space.ro(&vm.cell_key).lookup("/test").is_some());
    /// ```
    pub fn initialize_string<P, S>(&mut self, code: S, filename: P) -> Result<TaskContext>
    where
        P: AsRef<Path>,
        S: AsRef<str>,
    {
        let lpc_path = LpcPath::new_in_game(filename.as_ref(), "/", &*self.config.lib_dir);
        self.config.validate_in_game_path(&lpc_path, None)?;

        self.with_compiler(|compiler, cell_key| compiler.compile_string(lpc_path, code, cell_key))
            .and_then(|program| self.create_and_initialize_task(program))
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
    }

    /// Run a callback with a new, initialized [`Compiler`].
    fn with_compiler<F, T>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(Compiler, &mut QCellOwner) -> Result<T>,
    {
        let object_space = self.object_space.ro(&self.cell_key);
        let compiler = CompilerBuilder::default()
            .config(self.config.clone())
            .simul_efuns(get_simul_efuns(&self.config, object_space))
            .build()?;
        f(compiler, &mut self.cell_key)
    }

    /// Create a new [`Task`] and initialize it with the given [`Program`].
    fn create_and_initialize_task(&mut self, program: Program) -> Result<TaskContext> {
        Task::<MAX_CALL_STACK_SIZE>::initialize_program(
            program,
            self.config.clone(),
            self.object_space.clone(),
            self.memory.clone(),
            self.upvalues.clone(),
            self.call_outs.clone(),
            self.tx.clone(),
            &mut self.cell_key,
        )
        .map(|task| {
            let process = task.context.process();
            ObjectSpace::insert_process(&self.object_space, process, &mut self.cell_key);

            task.context
        })
    }
}

impl Mark for Vm {
    #[instrument(skip(self, cell_key))]
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut BitSet,
        cell_key: &QCellOwner,
    ) -> Result<()> {
        // TODO: mark all tasks
        self.object_space
            .ro(cell_key)
            .mark(marked, processed, cell_key)?;

        self.call_outs
            .ro(cell_key)
            .mark(marked, processed, cell_key)
    }
}

impl KeylessSweep for Vm {
    #[instrument(skip(self))]
    #[inline]
    fn keyless_sweep(&mut self, marked: &BitSet) -> Result<()> {
        self.upvalues.rw(&mut self.cell_key).keyless_sweep(marked)
    }
}

impl<'a> Keyable<'a> for Vm {
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(
            f,
            "Vm {{ object_space: {:?}, memory {:?}, upvalues: {:?}, config: {:?} }}",
            self.object_space.ro(cell_key).with_key(cell_key),
            self.memory,
            self.upvalues.ro(cell_key),
            self.config
        )
    }

    fn keyable_hash<H: Hasher>(&self, _state: &mut H, _cell_key: &QCellOwner) {
        unimplemented!()
    }

    fn keyable_eq(&self, _other: &Self, _cell_key: &QCellOwner) -> bool {
        unimplemented!()
    }

    fn keyable_partial_cmp(&self, _other: &Self, _cell_key: &QCellOwner) -> Option<Ordering> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::test_support::test_config;

    #[test]
    fn test_gc() {
        let cell_key = QCellOwner::new();
        let mut vm = Vm::new_with_key(test_config(), cell_key);
        let storage = indoc! { r#"
            function *storage = ({});

            void store(function f) {
                dump("storing", f);
                storage += ({ f });
            }

            void runem() {
                dump("running", storage);
                foreach (f: storage) {
                    f();
                }
            }
        "# };

        let runner = indoc! { r#"
            void create() {
                int i = -1;

                object storage = clone_object("/storage");

                dump("storage", storage);

                while(++i < 5) {
                    storage->store((:
                        dump("yo", i);
                    :));
                }

                storage->runem();
            }
        "# };

        let ctx1 = vm
            .initialize_string(storage, "storage")
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();
        let _ctx2 = vm
            .initialize_string(runner, "runner")
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();

        assert_eq!(ctx1.upvalues().ro(&vm.cell_key).len(), 1);

        vm.gc().unwrap();

        assert_eq!(ctx1.upvalues().ro(&vm.cell_key).len(), 1);

        vm.object_space.rw(&mut vm.cell_key).clear();

        vm.gc().unwrap();

        assert_eq!(ctx1.upvalues().ro(&vm.cell_key).len(), 0);
    }
}
