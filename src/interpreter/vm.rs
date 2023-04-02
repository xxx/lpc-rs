use std::{
    cmp::Ordering,
    fmt::Formatter,
    hash::Hasher,
    path::Path,
    rc::Rc,
    sync::mpsc::{Receiver, Sender},
};

use bit_set::BitSet;
use educe::Educe;
use if_chain::if_chain;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use lpc_rs_utils::config::Config;
use qcell::{QCell, QCellOwner};
use tracing::{instrument, trace};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::{Compiler, CompilerBuilder},
    interpreter::{
        call_outs::{CallOut, CallOuts},
        efun::EFUN_PROTOTYPES,
        function_type::function_address::FunctionAddress,
        gc::{
            gc_bank::{GcBank, GcRefBank},
            mark::Mark,
            sweep::{KeylessSweep, Sweep},
        },
        lpc_ref::{LpcRef, NULL},
        lpc_value::LpcValue,
        memory::Memory,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::Task,
        task_context::TaskContext,
        vm_op::VmOp,
    },
    try_extract_value,
    util::{get_simul_efuns, keyable::Keyable, qcell_debug},
};

#[derive(Educe)]
#[educe(Debug)]
#[readonly::make]
pub struct Vm {
    /// Our object space, which stores all of the system objects (masters and clones)
    #[educe(Debug(method = "qcell_debug"))]
    pub object_space: Rc<QCell<ObjectSpace>>,

    /// Shared VM memory
    memory: Memory,

    /// All upvalues are stored in the [`Vm`], and are shared between all [`Task`]s
    #[educe(Debug(method = "qcell_debug"))]
    pub upvalues: Rc<QCell<GcRefBank>>,

    /// The [`Config`] that's in use for this [`Vm`]
    config: Rc<Config>,

    /// Enqueued call outs
    #[educe(Debug(method = "qcell_debug"))]
    call_outs: Rc<QCell<CallOuts>>,

    /// The channel used to send [`VmOp`]s to this [`Vm`]
    tx: Sender<VmOp>,

    /// The channel used to receive [`VmOp`]s from other locations
    rx: Receiver<VmOp>,
}

impl Vm {
    /// Create a new [`Vm`].
    pub fn new<C>(config: C, cell_key: &QCellOwner) -> Self
    where
        C: Into<Rc<Config>>,
    {
        let object_space = ObjectSpace::default();
        let (tx, rx) = std::sync::mpsc::channel();
        let call_outs = cell_key.cell(CallOuts::new(tx.clone()));
        Self {
            object_space: Rc::new(cell_key.cell(object_space)),
            memory: Memory::default(),
            config: config.into(),
            upvalues: Rc::new(cell_key.cell(GcBank::default())),
            call_outs: Rc::new(call_outs),
            rx,
            tx,
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
    pub fn boot(&mut self, cell_key: &mut QCellOwner) -> Result<()> {
        self.bootstrap(cell_key)?;

        self.run(cell_key)?;

        Ok(())
    }

    /// Load and initialize the master object and simul_efuns.
    ///
    /// # Arguments
    ///
    /// * `cell_key` - The [`QCellOwner`] that will be used to create _all_ [`QCell`]s
    ///                in the system.
    ///
    /// # Returns
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the master object
    /// * `Err(LpcError)` - If there was an error.
    pub fn bootstrap(&mut self, cell_key: &mut QCellOwner) -> Result<TaskContext> {
        if let Some(Err(e)) = self.initialize_simul_efuns(cell_key) {
            e.emit_diagnostics();
            return Err(e);
        }

        let master_path =
            LpcPath::new_in_game(&*self.config.master_object, "/", &*self.config.lib_dir);
        self.initialize_file(&master_path, cell_key)
    }

    /// Run the [`Vm`]'s main loop.
    /// Assumes `boot()` has already been called.
    /// # Arguments
    ///
    /// * `cell_key` - The [`QCellOwner`] that will be used to create _all_ [`QCell`]s
    ///                in the system.
    #[instrument(skip_all)]
    pub fn run(&mut self, cell_key: &mut QCellOwner) -> Result<()> {
        loop {
            match self.rx.recv() {
                Ok(op) => {
                    trace!(?op, "Received VmOp");

                    match op {
                        VmOp::RunCallOut(idx) => {
                            self.op_run_call_out(idx, cell_key)?;
                        }
                    }
                }
                Err(e) => {
                    panic!("Error receiving from channel: {}", e);
                }
            }
        }
        //
        // Ok(())
    }

    /// Handler for [`VmOp::RunCallOut`]
    fn op_run_call_out(&mut self, idx: usize, cell_key: &mut QCellOwner) -> Result<TaskContext> {
        let mut repeating = false;
        let (process, function, args) = if_chain! {
            if let Some(call_out) = self.call_outs.ro(cell_key).get(idx);
            if let LpcRef::Function(ref func) = call_out.func_ref;
            then {
                repeating = call_out.repeating;
                let b = func.borrow();
                let ptr = try_extract_value!(&*b, LpcValue::Function);

                // call outs don't get any additional args passed to them, so just set up the partial args.
                // use int 0 for any that were not applied at the time the pointer was created
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
                        let Some(simul_efuns) = get_simul_efuns(&self.config, self.object_space.ro(cell_key)) else {
                            return Err(LpcError::new_bug(
                                "function pointer to simul_efun passed, but no simul_efuns?".to_string(),
                            ));
                        };

                        let Some(function) = simul_efuns.ro(cell_key).program.lookup_function(name) else {
                            return Err(LpcError::new(format!("call to unknown simul_efun `{name}`")));
                        };

                        (simul_efuns.clone(), function.clone(), args)
                    },
                    FunctionAddress::Efun(name) => {
                        let prototype = EFUN_PROTOTYPES.get(name.as_str()).unwrap();
                        // TODO: prototypes should be in Rcs so this clone is cheap
                        let pf = ProgramFunction::new(prototype.clone(), 0);

                        (Rc::new(cell_key.cell(Process::default())), Rc::new(pf), args)
                    }
                }
            }
            else {
                return Err(LpcError::new("invalid function sent to `call_out`"));
            }
        };

        let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&self.memory, self.upvalues.clone());

        let task_context = TaskContext::new(
            self.config.clone(),
            process,
            self.object_space.clone(),
            self.upvalues.clone(),
            self.call_outs.clone(),
            self.tx.clone(),
            cell_key,
        );

        let result = task.eval(function, &args, task_context, cell_key);
        trace!(?result, "call out finished");

        if !repeating {
            let _ = self.call_outs.rw(cell_key).remove(idx);
        }

        result
    }

    /// Initialize the simulated efuns file, if it is configured.
    ///
    /// # Returns
    ///
    /// * `Some(Ok(TaskContext))` - The [`TaskContext`] for the simul_efun file
    /// * `Some(Err(LpcError))` - If there was an error loading the simul_efun file
    /// * `None` - If there is no simul_efun file configured
    pub fn initialize_simul_efuns(
        &mut self,
        cell_key: &mut QCellOwner,
    ) -> Option<Result<TaskContext>> {
        let Some(path) = &self.config.simul_efun_file else {
            return None
        };

        let simul_efun_path = LpcPath::new_in_game(path.as_str(), "/", &*self.config.lib_dir);
        Some(self.initialize_file(&simul_efun_path, cell_key))
    }

    /// Do a full garbage collection cycle.
    #[instrument(skip_all)]
    pub fn gc(&mut self, cell_key: &mut QCellOwner) -> Result<()> {
        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        self.mark(&mut marked, &mut processed, cell_key).unwrap();

        trace!("Marked {} objects", marked.len());

        self.sweep(&marked, cell_key)
    }

    /// Compile and initialize code from the passed file.
    fn initialize_file(
        &mut self,
        filename: &LpcPath,
        cell_key: &mut QCellOwner,
    ) -> Result<TaskContext> {
        debug_assert!(matches!(filename, &LpcPath::InGame(_)));
        let tx = self.tx.clone();

        self.with_compiler(cell_key, |compiler, cell_key| {
            compiler.compile_in_game_file(filename, None, cell_key)
        })
        .and_then(|program| self.create_and_initialize_task(program, tx, cell_key))
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
    /// let mut cell_key = QCellOwner::new();
    /// let mut vm = Vm::new(Config::default(), &cell_key);
    /// let ctx = vm
    ///     .initialize_string("int x = 5;", "test.c", &mut cell_key)
    ///     .unwrap();
    ///
    /// assert_eq!(
    ///     ctx.process().ro(&cell_key).globals.registers[0],
    ///     LpcRef::Int(5)
    /// );
    /// assert!(vm.object_space.ro(&cell_key).lookup("/test").is_some());
    /// ```
    pub fn initialize_string<P, S>(
        &mut self,
        code: S,
        filename: P,
        cell_key: &mut QCellOwner,
    ) -> Result<TaskContext>
    where
        P: AsRef<Path>,
        S: AsRef<str>,
    {
        let lpc_path = LpcPath::new_in_game(filename.as_ref(), "/", &*self.config.lib_dir);
        self.config.validate_in_game_path(&lpc_path, None)?;
        let tx = self.tx.clone();

        self.with_compiler(cell_key, |compiler, cell_key| {
            compiler.compile_string(lpc_path, code, cell_key)
        })
        .and_then(|program| self.create_and_initialize_task(program, tx, cell_key))
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
    }

    /// Run a callback with a new, initialized [`Compiler`].
    fn with_compiler<F, T>(&self, cell_key: &mut QCellOwner, f: F) -> Result<T>
    where
        F: FnOnce(Compiler, &mut QCellOwner) -> Result<T>,
    {
        let object_space = self.object_space.ro(cell_key);
        let compiler = CompilerBuilder::default()
            .config(self.config.clone())
            .simul_efuns(get_simul_efuns(&self.config, object_space))
            .build()?;
        f(compiler, cell_key)
    }

    /// Create a new [`Task`] and initialize it with the given [`Program`].
    fn create_and_initialize_task(
        &mut self,
        program: Program,
        tx: Sender<VmOp>,
        cell_key: &mut QCellOwner,
    ) -> Result<TaskContext> {
        let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&self.memory, self.upvalues.clone());

        task.initialize_program(
            program,
            self.config.clone(),
            self.object_space.clone(),
            self.call_outs.clone(),
            tx,
            cell_key,
        )
        .map(|ctx| {
            let process = ctx.process();
            ObjectSpace::insert_process(&self.object_space, process, cell_key);

            ctx
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
            .mark(marked, processed, cell_key)
    }
}

impl Sweep for Vm {
    #[instrument(skip(self, cell_key))]
    #[inline]
    fn sweep(&mut self, marked: &BitSet, cell_key: &mut QCellOwner) -> Result<()> {
        self.upvalues.rw(cell_key).keyless_sweep(marked)
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
        let mut cell_key = QCellOwner::new();
        let mut vm = Vm::new(test_config(), &cell_key);
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
            .initialize_string(storage, "storage", &mut cell_key)
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();
        let _ctx2 = vm
            .initialize_string(runner, "runner", &mut cell_key)
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();

        assert_eq!(ctx1.upvalues().ro(&cell_key).len(), 1);

        vm.gc(&mut cell_key).unwrap();

        assert_eq!(ctx1.upvalues().ro(&cell_key).len(), 1);

        vm.object_space.rw(&mut cell_key).clear();

        vm.gc(&mut cell_key).unwrap();

        assert_eq!(ctx1.upvalues().ro(&cell_key).len(), 0);
    }
}
