use std::sync::Arc;

use bit_set::BitSet;
use flume::Sender as FlumeSender;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::{
    signal,
    sync::mpsc::{error::SendError, Receiver, Sender},
};
use tracing::{debug, error, info, instrument, trace};
use vm_op::VmOp;

use crate::{
    compile_time_config::VM_CHANNEL_CAPACITY,
    interpreter::{
        call_outs::CallOuts,
        gc::{
            gc_bank::{GcBank, GcRefBank},
            mark::Mark,
            sweep::KeylessSweep,
        },
        heap::Heap,
        object_space::ObjectSpace,
        process::Process,
        task::apply_function::apply_function_in_master,
        task_context::TaskContext,
        SHUTDOWN,
    },
    telnet::{
        connection::Connection,
        connection_broker::ConnectionBroker,
        ops::{BrokerOp, ConnectionOp},
        Telnet,
    },
    util::process_builder::ProcessInitializer,
};
use crate::interpreter::task::apply_function::apply_runtime_error;

mod initiate_login;
mod object_initializers;
mod prioritize_call_out;

pub mod vm_op;

#[derive(Debug)]
#[readonly::make]
pub struct Vm {
    /// Our object space, which stores all of the system objects (masters and clones)
    pub object_space: Arc<ObjectSpace>,

    /// Shared VM memory. Reference-type `LpcRef`s are allocated out of this.
    pub memory: Arc<Heap>,

    /// All upvalues are stored in the [`Vm`], and are shared between all [`Task`](crate::interpreter::task::Task)s
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
    pub async fn boot(&mut self) -> lpc_rs_errors::Result<()> {
        self.bootstrap().await?;

        let address = format!("{}:{}", self.config.bind_address, self.config.port);
        self.connection_broker
            .run(address, self.new_task_template())
            .await;
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
        self.process_initialize_from_path(&master_path)
            .await
            .map(|t| t.context)
        // self.initialize_file(&master_path).await
    }

    /// Run the [`Vm`]'s main loop, which is the main event loop for the entire system.
    /// Assumes `bootstrap()` has already been called.
    /// This runs on the main execution thread, and should never do any work itself.
    /// Spawn a task to do anything beyond message handling, or logging.
    #[instrument(skip_all)]
    pub async fn run(&mut self) -> lpc_rs_errors::Result<()> {
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
                        VmOp::InitiateLogin(connection) => {
                            self.initiate_login(connection).await;
                        }
                        VmOp::PrioritizeCallOut(idx) => {
                            self.prioritize_call_out(idx).await;
                        }
                        VmOp::Exec(connection, process, callback) => {
                            // We do this here in the VM because there are multiple objects
                            // being updated at once, and we don't want to have to deal with
                            // locking them behind a mutex.
                            let prev = Self::takeover_process(connection, process).await;
                            let _ = callback.send(prev);
                        }
                        VmOp::RuntimeError(error, proc) => {
                            let template = self.new_task_template();

                            tokio::spawn(async move {
                                match apply_runtime_error(&error, proc, template).await {
                                    Some(Ok(_)) => {},
                                    None => {
                                        error!("runtime_error() is not defined in the master object.");
                                    }
                                    Some(Err(e)) => {
                                        error!("Error applying runtime error: {}", e.diagnostic_string());
                                    }
                                }
                            });
                        }
                        VmOp::TaskError(_task_id, error) => {
                            tokio::spawn(async move { error.emit_diagnostics() });
                        },
                        VmOp::FatalError(error) => {
                            error!("VM notified of fatal error: {}. Shutting down.", error);
                            break;
                        },
                    }
                }
            }
        }

        // Only the VM shuts down on its own. Everything else shuts down only at the behest of the VM.
        self.shutdown().await
    }

    /// Shut down the [`Vm`], and all subsystems.
    pub async fn shutdown(&mut self) -> Result<()> {
        // tell the broker to break out of its main loop.
        let _ = self.broker_tx.send_async(BrokerOp::Shutdown).await;

        self.connection_broker.disable_incoming_connections();
        self.call_outs.write().clear();

        match apply_function_in_master(
            SHUTDOWN,
            &[],
            self.new_task_template(),
            Some(5000), // a much longer timeout than normal, to allow for saving.
        )
        .await
        {
            Some(Ok(_)) => {
                debug!("shutdown() successfully applied in master object");
            }
            Some(Err(e)) => {
                error!("shutdown() in master object errored: {}", e);
            }
            None => {
                debug!("shutdown() not defined in the master object, so nothing to do");
            }
        }

        self.connection_broker.disconnect_users();

        Ok(())
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

    /// Send an operation to the VM queue
    pub async fn send_op(&self, msg: VmOp) -> std::result::Result<(), SendError<VmOp>> {
        self.tx.send(msg).await
    }

    /// Serialized public facade for the `exec` efun.
    /// The overall mechanism is to call this function, which will send an `VmOp::Exec` to the VM, and
    /// await the response. The `Exec` op is picked up, and will call `takeover_process`, which actually
    /// makes the switch, and then will return the previous connection, if there was one.
    ///
    /// # Arguments
    ///
    /// * `connection` - The [`Connection`] to attach to the [`Process`]
    /// * `process` - The [`Process`] to attach the [`Connection`] to
    /// * `vm_tx` - The channel to send the `Exec` op to
    ///
    /// # Returns
    ///
    /// * `Some(Arc<Connection>)` - The previous connection in `process`, if there was one
    /// * `None` - If there was no previous connection
    pub async fn exec(
        connection: Arc<Connection>,
        process: Arc<Process>,
        vm_tx: Sender<VmOp>,
    ) -> Option<Arc<Connection>> {
        let (exec_tx, exec_rx) = tokio::sync::oneshot::channel();
        let _ = vm_tx.send(VmOp::Exec(connection, process, exec_tx)).await;
        exec_rx.await.ok().flatten()
    }

    /// Set the [`Process`] that a [`Connection`] is connected to, and tag the
    /// [`Process`] with the [`Connection`].
    /// Drops the previous [`Connection`] that was attached to the [`Process`] if there was one.'
    /// This is the underlying mechanism of the `exec` efun.
    ///
    /// NOTE: No synchronization is done here. It's assumed that this is being called
    /// from behind a channel for serialization.
    async fn takeover_process(
        connection: Arc<Connection>,
        process: Arc<Process>,
    ) -> Option<Arc<Connection>> {
        // These are the swaps that need to be done atomically.
        connection.process.swap(Some(process.clone()));
        let previous = process.connection.swap(Some(connection));

        if let Some(conn) = &previous {
            let _ = conn
                .tx
                .send(ConnectionOp::SendMessage(
                    "You are being disconnected because someone else logged in as you.".to_string(),
                ))
                .await;
            let _ = conn
                .broker_tx
                .send_async(BrokerOp::Disconnect(conn.address))
                .await;
        }

        previous
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
