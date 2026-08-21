use std::sync::Arc;

use bit_set::BitSet;
use flume::Sender as FlumeSender;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_utils::config::Config;
use tokio::{
    signal,
    sync::mpsc::{Receiver, Sender, error::SendError},
};
use tracing::{debug, error, info, instrument, trace};
use vm_op::VmOp;

use crate::{
    compile_time_config::VM_CHANNEL_CAPACITY,
    interpreter::{
        SHUTDOWN,
        gc::mark::Mark,
        process::Process,
        stm::live_count,
        task::apply_function::{apply_function_in_master, apply_runtime_error},
        task_context::TaskContext,
        vm::global_state::GlobalState,
    },
    telnet::{Telnet, connection::Connection, connection_broker::ConnectionBroker, ops::BrokerOp},
    util::process_builder::ProcessInitializer,
};

mod initiate_login;
mod object_initializers;
mod prioritize_call_out;

pub mod global_state;
pub mod vm_op;

#[derive(Debug)]
#[readonly::make]
pub struct Vm {
    pub global_state: Arc<GlobalState>,

    /// The connection broker, which handles all of the network connections
    connection_broker: ConnectionBroker,

    // /// The channel used to send [`VmOp`]s to this [`Vm`]
    // tx: Sender<VmOp>,
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
        let (tx, rx) = tokio::sync::mpsc::channel(VM_CHANNEL_CAPACITY);
        let (broker_tx, broker_rx) = flume::bounded(VM_CHANNEL_CAPACITY);
        let telnet = Telnet::new(broker_tx.clone());

        Self {
            global_state: Arc::new(GlobalState::new(config, tx.clone())),
            connection_broker: ConnectionBroker::new(tx, broker_rx, telnet),
            rx,
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

        let address = format!("{}:{}", self.config().bind_address, self.config().port);
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
            LpcPath::new_in_game(&*self.config().master_object, "/", &*self.config().lib_dir);
        self.initialize_process_from_path(&master_path)
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
        self.global_state.with_call_outs_mut(|c| c.clear());

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

        // Stop the STM committer deterministically as the VM winds down,
        // after the master's shutdown hook has run. GlobalState's Drop will also
        // close + join; this just makes the ordering explicit.
        self.global_state.close_committer();

        self.connection_broker.disconnect_users();

        Ok(())
    }

    /// Do a full garbage collection cycle.
    ///
    /// # Precondition
    ///
    /// The committer must be **quiescent** — no transaction in flight. A live task *is* an
    /// in-flight transaction, so at quiescence the root set (object space + call-outs) is
    /// complete with no task or transaction log to mark. A non-quiescent call is refused
    /// rather than blocked: a concurrent task at a GC point is a caller bug.
    #[instrument(skip_all)]
    pub async fn gc(&self) -> Result<()> {
        let live = live_count(&self.global_state.committer_tx).await?;
        if live != 0 {
            return Err(lpc_error!(
                "gc refused: committer not quiescent ({live} transaction(s) in flight)"
            ));
        }

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        self.mark(&mut marked, &mut processed)?;

        trace!("Marked {} objects", marked.count());

        self.sweep(&marked)
    }

    /// Send an operation to the VM queue
    pub async fn send_op(&self, msg: VmOp) -> std::result::Result<(), SendError<VmOp>> {
        self.tx().send(msg).await
    }

    #[instrument(skip(self))]
    #[inline]
    pub fn sweep(&self, marked: &BitSet) -> Result<()> {
        self.global_state.sweep(marked)
    }

    #[inline]
    fn config(&self) -> &Config {
        &self.global_state.config
    }

    #[inline]
    fn tx(&self) -> &Sender<VmOp> {
        &self.global_state.tx
    }

    /// Bind a [`Connection`] to a [`Process`], transactionally.
    ///
    /// Used by the login path (`initiate_login`). The connection cell on
    /// `process` is written in its own transaction against the committer
    /// (not inside any task's transaction), so it commits independently.
    /// The physical socket-level handover — the connection's back-reference
    /// pointing at `process`, and the disconnect of whatever was previously
    /// bound — is a deferred `Effect::Exec`, flushed only after the commit
    /// lands.
    pub async fn takeover(
        global_state: &Arc<GlobalState>,
        connection: Arc<Connection>,
        process: Arc<Process>,
    ) {
        use crate::interpreter::stm::{
            Effect, Transaction, commit_changeset, flush_effects, start_txn,
        };

        let committer_tx = &global_state.committer_tx;

        loop {
            let live = match start_txn(committer_tx).await {
                Ok(l) => l,
                Err(e) => {
                    error!("takeover: committer start failed: {e}");
                    return;
                }
            };

            let mut txn = Transaction::new(live.inner.clone());

            // The connection currently bound to `process` (if any); the
            // handover displaces it.
            let previous = txn.read_connection(process.connection.id);
            txn.write_connection(process.connection.id, Some(connection.clone()));

            txn.record_effect(Effect::Exec {
                new_process: process.clone(),
                connection: connection.clone(),
                previous,
            });

            let effects = txn.take_effects();
            let (_world, changeset) = txn.into_parts();

            let commit = commit_changeset(committer_tx, changeset).await;
            drop(live);

            match commit {
                Ok(Ok(())) => {
                    flush_effects(
                        &global_state.config,
                        &global_state.object_space,
                        global_state.call_outs(),
                        effects,
                    )
                    .await;
                    return;
                }
                Ok(Err(_)) => continue,
                Err(e) => {
                    tracing::error!("takeover: committer commit failed: {e}");
                    return;
                }
            }
        }
    }
}

impl Mark for Vm {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        self.global_state.mark(marked, processed)
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, stm::start_txn},
        test_support::test_config,
    };

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
            .inspect_err(|e| {
                e.emit_diagnostics();
            })
            .unwrap();
        let _ctx2 = vm
            .initialize_string(runner, "runner")
            .await
            .inspect_err(|e| {
                e.emit_diagnostics();
            })
            .unwrap();

        let assert_len = |ctx: &TaskContext, len| {
            ctx.global_state.with_upvalues(|uv| {
                assert_eq!(uv.len(), len);
            });
        };

        assert_len(&ctx1, 1);

        vm.gc().await.unwrap();

        assert_len(&ctx1, 0);

        vm.global_state.object_space.clear();

        vm.gc().await.unwrap();

        assert_len(&ctx1, 0);
    }

    /// A GC pass is refused while a transaction is in flight. The transaction
    /// is pinned by holding a [`crate::interpreter::stm::LiveSnapshot`] across
    /// the `gc` call.
    #[tokio::test]
    async fn gc_refuses_while_transaction_in_flight() {
        let vm = Vm::new(test_config());
        let tx = vm.global_state.committer_tx.clone();

        // Baseline: nothing in flight, so a sweep goes through.
        vm.gc().await.unwrap();

        // Pin a transaction so the committer is not quiescent.
        let live = start_txn(&tx).await.unwrap();

        let err = vm.gc().await.unwrap_err();
        assert!(
            err.to_string().contains("not quiescent"),
            "expected a non-quiescent refusal, got: {err}"
        );

        drop(live);

        vm.gc().await.unwrap();
    }

    /// One transaction creates and destructs a prototype repeatedly, and
    /// returns the handles it produced. The destructed ones must not
    /// resurrect from the physical map or the committed world (the removal is
    /// deferred to commit, so both still hold the old process until then):
    /// each re-create is a fresh object, only the last survives the commit,
    /// and it is the one the physical map and the committed world agree on.
    #[tokio::test]
    async fn test_create_destruct_cycles_one_transaction() {
        let mut vm = Vm::new(test_config());

        let code = indoc! { r##"
            mixed *create() {
                object a = find_object("/example2");
                destruct(a);
                object b = find_object("/example2");
                destruct(b);
                object c = find_object("/example2");
                destruct(c);
                object d = find_object("/example2");
                return ({ a, b, c, d });
            }
        "## };

        let ctx = vm
            .initialize_string(code, "driver.c")
            .await
            .inspect_err(|e| e.emit_diagnostics())
            .expect("driver init failed");

        let LpcRef::Array(cell) = ctx.result().expect("create() must return the array") else {
            panic!("expected a 4-element array, got {:?}", ctx.result());
        };
        let arr = ctx
            .global_state
            .committed_array(cell.id)
            .expect("array payload committed");
        assert_eq!(arr.len(), 4, "array must hold the four handles");

        let handle = |i: usize| -> std::sync::Weak<Process> {
            let LpcRef::Object(h) = &arr[i] else {
                panic!("element {i} is not an object");
            };
            h.clone()
        };

        let physical = ctx
            .global_state
            .object_space
            .lookup("/example2")
            .expect("committed world must hold the surviving prototype");

        // The three destructed prototypes are gone for good: their only
        // strong roots were the cell (dropped) and the physical map
        // (RemoveObject flushed at commit).
        for i in 0..3 {
            assert!(
                handle(i).upgrade().is_none(),
                "handle {i} (a destructed prototype) must be dead after the commit"
            );
        }

        // The last re-created prototype is alive and is exactly the one the
        // physical map holds: not a resurrection of any earlier cycle.
        let live = handle(3)
            .upgrade()
            .expect("the last-created prototype must still be alive");
        assert!(
            std::sync::Arc::ptr_eq(&physical, &live),
            "the committed /example2 must be the last-created object, not an earlier cycle"
        );
    }
}
