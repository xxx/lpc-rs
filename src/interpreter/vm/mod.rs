use std::{path::Path, sync::Arc};

use bit_set::BitSet;
use flume::Sender as FlumeSender;
use if_chain::if_chain;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::{
    signal,
    sync::mpsc::{Receiver, Sender},
};
use tracing::{info, instrument, trace};
use vm_op::VmOp;

use crate::{
    compile_time_config::{MAX_CALL_STACK_SIZE, VM_CHANNEL_CAPACITY},
    compiler::{Compiler, CompilerBuilder},
    interpreter::{
        call_outs::CallOuts,
        efun::EFUN_FUNCTIONS,
        function_type::function_address::FunctionAddress,
        gc::{
            gc_bank::{GcBank, GcRefBank},
            mark::Mark,
            sweep::KeylessSweep,
        },
        lpc_ref::{LpcRef, NULL},
        heap::Heap,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{task_id::TaskId, Task},
        task_context::TaskContext,
    },
    telnet::{connection_broker::ConnectionBroker, ops::BrokerOp, Telnet},
    util::get_simul_efuns,
};

pub mod vm_op;

#[derive(Debug)]
#[readonly::make]
pub struct Vm {
    /// Our object space, which stores all of the system objects (masters and clones)
    pub object_space: Arc<ObjectSpace>,

    /// Shared VM memory. Reference-type `LpcRef`s are allocated out of this.
    memory: Arc<Heap>,

    /// All upvalues are stored in the [`Vm`], and are shared between all [`Task`]s
    pub upvalues: Arc<RwLock<GcRefBank>>,

    /// The [`Config`] that's in use for this [`Vm`]
    config: Arc<Config>,

    /// Enqueued call outs
    call_outs: Arc<RwLock<CallOuts>>,

    /// The connection broker, which handles all of the network connections
    connection_broker: ConnectionBroker,

    /// The channel used to send [`VmOp`]s to this [`Vm`]
    tx: Sender<VmOp>,

    /// The channel used to receive [`VmOp`]s from other locations
    rx: Receiver<VmOp>,

    /// The channel used to send [`BrokerOp`]s to the connection broker
    broker_tx: FlumeSender<BrokerOp>,
}

impl Vm {
    /// Create a new [`Vm`].
    pub fn new<C>(config: C) -> Self
    where
        C: Into<Arc<Config>>,
    {
        let object_space = ObjectSpace::default();
        let (tx, rx) = tokio::sync::mpsc::channel(VM_CHANNEL_CAPACITY);
        let (broker_tx, broker_rx) = flume::bounded(VM_CHANNEL_CAPACITY);
        let call_outs = RwLock::new(CallOuts::new(tx.clone()));
        let telnet = Telnet::new(broker_tx.clone());

        Self {
            object_space: Arc::new(object_space),
            memory: Arc::new(Heap::default()),
            config: config.into(),
            upvalues: Arc::new(RwLock::new(GcBank::default())),
            call_outs: Arc::new(call_outs),
            connection_broker: ConnectionBroker::new(tx.clone(), broker_rx, telnet),
            rx,
            tx,
            broker_tx,
        }
    }

    /// The main initialization method for the VM.
    ///
    /// This method will load the master object and simul_efun file, add
    /// the master object to the object space, start networking,
    /// and then start the main loop.
    pub async fn boot(&mut self) -> Result<()> {
        self.bootstrap().await?;

        let address = format!("{}:{}", self.config.bind_address, self.config.port);
        self.connection_broker.run(address).await;
        self.run().await
    }

    /// Load and initialize the master object and simul_efuns.
    ///
    /// # Returns
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the master object
    /// * `Err(LpcError)` - If there was an error.
    pub async fn bootstrap(&mut self) -> Result<TaskContext> {
        if let Some(Err(e)) = self.initialize_simul_efuns().await {
            e.emit_diagnostics();
            return Err(e);
        }

        let master_path =
            LpcPath::new_in_game(&*self.config.master_object, "/", &*self.config.lib_dir);
        self.initialize_file(&master_path).await
    }

    /// Run the [`Vm`]'s main loop.
    /// Assumes `bootstrap()` has already been called.
    #[instrument(skip_all)]
    pub async fn run(&mut self) -> Result<()> {
        loop {
            tokio::select! {
                biased; // we want signal handlers checked first, always.
                _ = signal::ctrl_c() => {
                    // SIGINT on Linux
                    info!("Ctrl-C received... shutting down");
                    break;
                }
                Some(op) = self.rx.recv() => {
                    match op {
                        VmOp::PrioritizeCallOut(idx) => {
                            self.op_prioritize_call_out(idx).await;
                        }
                        VmOp::TaskComplete(_task_id) => {
                            // println!("task {task_id} complete");
                            // self.op_task_complete(task_id)?;
                        },
                        VmOp::TaskError(_task_id, error) => {
                            tokio::spawn(async move { error.emit_diagnostics() });
                        },
                        VmOp::Connected(connection) => {
                            info!("Vm connected: {:?}", connection);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Handler for [`VmOp::PrioritizeCallOut`].
    ///
    /// # Arguments
    ///
    /// * `idx` - The index of the call out to run
    ///
    /// Errors are communicated directly to the [`Vm`] via it's channel.
    async fn op_prioritize_call_out(&mut self, idx: usize) {
        let call_outs = self.call_outs.clone();
        let config = self.config.clone();
        let object_space = self.object_space.clone();
        let memory = self.memory.clone();
        let upvalues = self.upvalues.clone();
        let tx = self.tx.clone();

        tokio::spawn(async move {
            if call_outs.read().get(idx).is_none() {
                return;
            }

            let repeating: bool;
            let pair = {
                if_chain! {
                    let b = call_outs.read();
                    let call_out = b.get(idx).unwrap();
                    if let LpcRef::Function(ref func) = call_out.func_ref;
                    then {
                        repeating = call_out.is_repeating();
                        Ok((func.clone(), repeating))
                    } else {
                        Err(LpcError::new("invalid function sent to `call_out`"))
                    }
                }
            };

            let Ok((ptr_lock, repeating)) = pair else {
                call_outs.write().remove(idx);
                let _ = tx.send(VmOp::TaskError(TaskId(0), pair.unwrap_err())).await;
                return;
            };

            let triple = {
                let ptr = ptr_lock.read();

                // call outs don't get any additional args passed to them, so just set up the partial args.
                // use int 0 for any that were not applied at the time the pointer was created
                // TODO: error instead of int 0?
                let args = ptr
                    .partial_args
                    .iter()
                    .map(|arg| match arg {
                        Some(lpc_ref) => lpc_ref.clone(),
                        None => NULL,
                    })
                    .collect::<Vec<_>>();

                match ptr.address {
                    FunctionAddress::Local(ref proc, ref function) => {
                        if let Some(proc) = proc.upgrade() {
                            Ok((proc, function.clone(), args))
                        } else {
                            Err(LpcError::new(
                                "attempted to prioritize a function pointer to a dead process",
                            ))
                        }
                    }
                    FunctionAddress::Dynamic(_) => Err(LpcError::new(
                        "attempted to prioritize a dynamic receiver passed to call_out",
                    )),
                    FunctionAddress::SimulEfun(name) => {
                        match get_simul_efuns(&config, &object_space) {
                            Some(simul_efuns) => {
                                let b = simul_efuns.read();
                                match b.program.lookup_function(name) {
                                    Some(function) => {
                                        Ok((simul_efuns.clone(), function.clone(), args))
                                    }
                                    None => Err(LpcError::new(format!(
                                        "call to unknown simul_efun `{name}`"
                                    ))),
                                }
                            }
                            None => Err(LpcError::new(
                                "function pointer to simul_efun passed, but no simul_efuns?",
                            )),
                        }
                    }
                    FunctionAddress::Efun(name) => {
                        let pf = EFUN_FUNCTIONS.get(name.as_str()).unwrap();

                        Ok((Arc::new(RwLock::new(Process::default())), pf.clone(), args))
                    }
                }
            };

            let Ok((process, function, args)) = triple else {
                call_outs.write().remove(idx);
                let _ = tx.send(VmOp::TaskError(TaskId(0), triple.unwrap_err())).await;
                return;
            };

            {
                let mut call_outs = call_outs.write();
                if repeating {
                    call_outs.get_mut(idx).unwrap().refresh();
                } else {
                    call_outs.remove(idx);
                }
            }

            let task_context = TaskContext::new(
                config,
                process,
                object_space,
                memory,
                upvalues,
                call_outs,
                tx.clone(),
            );

            let mut task = Task::<MAX_CALL_STACK_SIZE>::new(task_context);
            let id = task.id;

            if let Err(e) = task.timed_eval(function, &args).await {
                let _ = tx
                    .send(VmOp::TaskError(
                        id,
                        e.with_stack_trace(task.stack.stack_trace()),
                    ))
                    .await;
            }
        });
    }

    /// Initialize the simulated efuns file, if it is configured.
    ///
    /// # Returns
    ///
    /// * `Some(Ok(TaskContext))` - The [`TaskContext`] for the simul_efun file
    /// * `Some(Err(LpcError))` - If there was an error loading the simul_efun file
    /// * `None` - If there is no simul_efun file configured
    pub async fn initialize_simul_efuns(&mut self) -> Option<Result<TaskContext>> {
        let Some(path) = &self.config.simul_efun_file else {
            return None
        };

        let simul_efun_path = LpcPath::new_in_game(path.as_str(), "/", &*self.config.lib_dir);
        Some(self.initialize_file(&simul_efun_path).await)
    }

    /// Do a full garbage collection cycle.
    #[instrument(skip_all)]
    pub fn gc(&mut self) -> Result<()> {
        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        self.mark(&mut marked, &mut processed).unwrap();

        trace!("Marked {} objects", marked.len());

        self.keyless_sweep(&marked)
    }

    /// Compile and initialize code from the passed file.
    async fn initialize_file(&mut self, filename: &LpcPath) -> Result<TaskContext> {
        debug_assert!(matches!(filename, &LpcPath::InGame(_)));

        let program =
            self.with_compiler(|compiler| compiler.compile_in_game_file(filename, None))?;

        self.create_and_initialize_task(program).await.map_err(|e| {
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
    ///
    /// # Returns
    ///
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the code
    /// * `Err(LpcError)` - If there was an error compiling or initializing the code
    ///
    /// # Examples
    ///
    /// ```
    /// tokio_test::block_on(async {
    /// use lpc_rs::interpreter::{lpc_ref::LpcRef, vm::Vm, lpc_int::LpcInt};
    /// use lpc_rs_utils::config::Config;
    ///
    /// let mut vm = Vm::new(Config::default());
    /// let ctx = vm.initialize_string("int x = 5;", "test.c").await.unwrap();
    ///
    /// assert_eq!(ctx.process().read().globals.registers[0], LpcRef::Int(LpcInt(5)));
    /// assert!(vm.object_space.lookup("/test").is_some());
    /// # })
    /// ```
    pub async fn initialize_string<P, S>(&mut self, code: S, filename: P) -> Result<TaskContext>
    where
        P: AsRef<Path>,
        S: AsRef<str>,
    {
        let lpc_path = LpcPath::new_in_game(filename.as_ref(), "/", &*self.config.lib_dir);
        self.config.validate_in_game_path(&lpc_path, None)?;

        let prog = self.with_compiler(|compiler| compiler.compile_string(lpc_path, code))?;

        self.create_and_initialize_task(prog).await.map_err(|e| {
            e.emit_diagnostics();
            e
        })
    }

    /// Run a callback with a new, initialized [`Compiler`].
    fn with_compiler<F, T>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(Compiler) -> Result<T>,
    {
        let compiler = CompilerBuilder::default()
            .config(self.config.clone())
            .simul_efuns(get_simul_efuns(&self.config, &self.object_space))
            .build()?;
        f(compiler)
    }

    /// Create a new [`Task`] and initialize it with the given [`Program`].
    async fn create_and_initialize_task(&mut self, program: Program) -> Result<TaskContext> {
        Task::<MAX_CALL_STACK_SIZE>::initialize_program(
            program,
            self.config.clone(),
            self.object_space.clone(),
            self.memory.clone(),
            self.upvalues.clone(),
            self.call_outs.clone(),
            self.tx.clone(),
        )
        .await
        .map(|task| {
            let process = task.context.process();
            ObjectSpace::insert_process(&self.object_space, process);

            task.context
        })
    }
}

impl Mark for Vm {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        // TODO: mark all tasks
        self.object_space.mark(marked, processed)?;

        self.call_outs.read().mark(marked, processed)
    }
}

impl KeylessSweep for Vm {
    #[instrument(skip(self))]
    #[inline]
    fn keyless_sweep(&mut self, marked: &BitSet) -> Result<()> {
        self.upvalues.write().keyless_sweep(marked)
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::test_support::test_config;

    #[tokio::test]
    async fn test_gc() {
        let mut vm = Vm::new(test_config());
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
            .await
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();
        let _ctx2 = vm
            .initialize_string(runner, "runner")
            .await
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();

        assert_eq!(ctx1.upvalues().read().len(), 1);

        vm.gc().unwrap();

        assert_eq!(ctx1.upvalues().read().len(), 1);

        vm.object_space.clear();

        vm.gc().unwrap();

        assert_eq!(ctx1.upvalues().read().len(), 0);
    }
}
