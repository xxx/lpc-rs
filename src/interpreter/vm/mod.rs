use std::sync::Arc;

use flume::Sender as FlumeSender;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
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
        process::Process,
        stm::{
            AttemptBody, Changeset, CommitProtocol, CommitterStats, Effect, LiveSnapshot,
            Transaction, commit_changeset, committer_stats, flush_effects, run_attempts, start_txn,
        },
        task::apply_function::{apply_function_in_master, apply_runtime_error},
        task_context::TaskContext,
        vm::global_state::GlobalState,
    },
    telnet::{Telnet, connection::Connection, connection_broker::ConnectionBroker, ops::BrokerOp},
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

    /// The committer's lifetime commit totals (commits/conflicts/errors).
    /// For bench measurement and tooling; not part of the hot path.
    pub async fn committer_stats(&self) -> Result<CommitterStats> {
        committer_stats(&self.global_state.committer_tx).await
    }

    /// Send an operation to the VM queue
    pub async fn send_op(&self, msg: VmOp) -> std::result::Result<(), SendError<VmOp>> {
        self.tx().send(msg).await
    }

    #[inline]
    fn config(&self) -> &Config {
        &self.global_state.config
    }

    #[inline]
    fn tx(&self) -> &Sender<VmOp> {
        &self.global_state.tx
    }

    /// Bind a [`Connection`] to a [`Process`] in its own transaction. The
    /// socket-level handover (back-reference, disconnect of the displaced
    /// holder) is a deferred `Effect::Exec`, flushed after the commit lands.
    pub async fn takeover(
        global_state: &Arc<GlobalState>,
        connection: Arc<Connection>,
        process: Arc<Process>,
    ) {
        let mut body = TakeoverBody {
            global_state: global_state.clone(),
            connection,
            process,
            attempt: None,
        };
        let (res, stats) = run_attempts(&global_state.committer_tx, &mut body).await;
        trace!(
            attempts = stats.attempts,
            conflicts = stats.conflicts,
            ?stats.duration,
            "takeover finished"
        );
        if let Err(e) = res {
            error!("takeover: committer failed: {e}");
        }
    }
}

/// One attempt of [`Vm::takeover`]: the connection-cell write plus the
/// deferred socket handover.
struct TakeoverBody {
    global_state: Arc<GlobalState>,
    connection: Arc<Connection>,
    process: Arc<Process>,
    attempt: Option<Transaction>,
}

#[async_trait::async_trait]
impl AttemptBody for TakeoverBody {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = start_txn(tx).await?;
        let mut txn = Transaction::new(live.inner.clone());

        // The connection currently bound to `process`; the handover
        // displaces it.
        let previous = txn.read_connection(self.process.connection.id);
        txn.write_connection(self.process.connection.id, Some(self.connection.clone()));

        txn.record_effect(Effect::Exec {
            new_process: self.process.clone(),
            connection: self.connection.clone(),
            previous,
        });

        self.attempt = Some(txn);
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Changeset>, Vec<Effect>)> {
        let mut txn = self
            .attempt
            .take()
            .expect("attempt present until committed");
        let commit = commit_changeset(tx, txn.take_changeset()).await?;
        Ok((commit, txn.take_effects()))
    }

    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        let gs = &self.global_state;
        flush_effects(&gs.config, &gs.object_space, gs.call_outs(), effects).await;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::net::ToSocketAddrs;

    use arc_swap::ArcSwapAny;
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader,
            lpc_ref::LpcRef,
            program::ProgramBuilder,
            stm::{Committer, WorldValue, start_txn},
            task::{apply_function::apply_function_by_name, task_template::TaskTemplateBuilder},
        },
        test_support::test_config,
    };

    /// A `Connection` whose own channels are dropped after the test.
    fn make_connection() -> Arc<Connection> {
        let (tx, _rx) = tokio::sync::mpsc::channel(1);
        let (broker_tx, _broker_rx) = flume::unbounded();
        Arc::new(Connection {
            address: "127.0.0.1:23123".to_socket_addrs().unwrap().next().unwrap(),
            process: ArcSwapAny::from(None),
            tx,
            broker_tx,
            input_to: Default::default(),
        })
    }

    /// A rejected takeover attempt re-runs: the second attempt commits the
    /// connection cell and only then flushes the back-reference.
    #[tokio::test]
    async fn takeover_reruns_after_rejection() {
        let (vm_tx, _vm_rx) = tokio::sync::mpsc::channel(128);
        let global_state = Arc::new(GlobalState::new(test_config(), vm_tx));
        let process = Arc::new(Process::new(
            ProgramBuilder::default()
                .filename(LpcPath::InGame(std::path::PathBuf::from("/body")))
                .build()
                .unwrap(),
        ));
        let connection = make_connection();

        let (tx, rx) = flume::bounded(4);
        let committer_tx = tx.clone();
        let handle =
            std::thread::spawn(move || Committer::new().run_with_rejections(committer_tx, rx, 1));

        let mut body = TakeoverBody {
            global_state: global_state.clone(),
            connection: connection.clone(),
            process: process.clone(),
            attempt: None,
        };
        let (res, stats) = run_attempts(&tx, &mut body).await;
        assert!(res.is_ok());
        assert_eq!(stats.attempts, 2, "one forced rejection, then a commit");
        assert_eq!(stats.conflicts, 1);
        assert!(
            connection
                .process
                .load()
                .as_ref()
                .is_some_and(|bound| Arc::ptr_eq(bound, &process)),
            "the back-reference is flushed after the commit"
        );

        tx.send(CommitProtocol::Close).unwrap();
        drop(tx);
        let world = handle.join().expect("committer panicked");
        assert!(
            matches!(
                world.read(process.connection.id),
                Some(WorldValue::Connection(Some(bound))) if Arc::ptr_eq(&bound, &connection)
            ),
            "the connection cell holds the committed binding"
        );
    }

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

        vm.global_state.gc().await.unwrap();

        assert_len(&ctx1, 0);

        vm.global_state.object_space.clear();

        vm.global_state.gc().await.unwrap();

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
        vm.global_state.gc().await.unwrap();

        // Pin a transaction so the committer is not quiescent.
        let live = start_txn(&tx).await.unwrap();

        let err = vm.global_state.gc().await.unwrap_err();
        assert!(
            err.to_string().contains("not quiescent"),
            "expected a non-quiescent refusal, got: {err}"
        );

        drop(live);

        vm.global_state.gc().await.unwrap();
    }

    /// A committed array still reachable from a live global survives the world
    /// sweep; one whose last live reference was dropped is reclaimed.
    #[tokio::test]
    async fn gc_reclaims_unreachable_committed_payload() {
        let mut vm = Vm::new(test_config());
        let code = indoc! { r##"
            mixed payload_a;
            mixed payload_b;

            void set_payloads() {
                payload_a = ({ 1, 2 });
                payload_b = ({ 3, 4 });
            }

            void drop_payload_a() {
                payload_a = 0;
            }
        "## };

        let ctx = vm.initialize_string(code, "gc_payload.c").await.unwrap();
        let proc = ctx.process;
        let template = TaskTemplateBuilder::default()
            .global_state(ctx.global_state.clone())
            .build()
            .unwrap();

        // Seed two live arrays into the world through a committed call. The
        // slot vars hold `LpcRef::Array` whose `SVar.id` is the payload cell.
        apply_function_by_name("set_payloads", &[], proc.clone(), template.clone(), None)
            .await
            .unwrap()
            .unwrap();

        // Extract each array's committed payload cell id from its slot.
        let cell_id = |proc: &Process| match vm.global_state.committed_global(proc, 0) {
            LpcRef::Array(svar) => svar.id,
            other => panic!("expected an array in slot 0, got {other:?}"),
        };
        let cell_a = cell_id(&proc);
        let cell_b = match vm.global_state.committed_global(&proc, 1) {
            LpcRef::Array(svar) => svar.id,
            other => panic!("expected an array in slot 1, got {other:?}"),
        };

        // Both cells live before any pass.
        assert!(vm.global_state.committed_array(cell_a).is_some());
        assert!(vm.global_state.committed_array(cell_b).is_some());

        // Reachable arrays survive the world sweep.
        vm.global_state.gc().await.unwrap();
        assert!(
            vm.global_state.committed_array(cell_b).is_some(),
            "a payload still reachable from a live global must survive gc"
        );
        assert!(vm.global_state.committed_array(cell_a).is_some());

        // Orphan the first array: its last live reference is gone.
        apply_function_by_name("drop_payload_a", &[], proc.clone(), template.clone(), None)
            .await
            .unwrap()
            .unwrap();

        vm.global_state.gc().await.unwrap();

        // The orphaned payload's cell is gone; the live one is not.
        assert!(
            vm.global_state.committed_array(cell_a).is_none(),
            "an orphaned committed array must be reclaimed by gc"
        );
        assert!(
            vm.global_state.committed_array(cell_b).is_some(),
            "a still-reachable committed array must survive gc"
        );
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

    /// The real `apply_function_by_name` path the bench drives, one worker: the committer-level
    /// probe already pinned 0 conflicts and lossless; this isolates the task layer.
    #[tokio::test]
    async fn contention_probe_sequential_increment_is_conflict_free() {
        const N: usize = 400;
        let mut vm = Vm::new(test_config());
        let code = indoc! { r#"
            int count = 0;

            void increment() {
                count = count + 1;
            }
        "# };
        let ctx = vm
            .initialize_string(code, "contention_probe_counter.c")
            .await
            .unwrap();
        let proc = ctx.process;
        let template = vm.new_task_template();

        let before = vm.committer_stats().await.unwrap();
        for _ in 0..N {
            apply_function_by_name("increment", &[], proc.clone(), template.clone(), None)
                .await
                .unwrap()
                .unwrap();
        }
        let after = vm.committer_stats().await.unwrap();
        let final_val = match vm.global_state.committed_global(&proc, 0) {
            LpcRef::Int(n) => n,
            other => panic!("counter not an int: {other:?}"),
        };

        let commits = after.commits - before.commits;
        let conflicts = after.conflicts - before.conflicts;
        println!(
            "[probe:task-seq] commits={commits} conflicts={conflicts} final={final_val} (expect 0 conflicts, final={N})"
        );

        assert_eq!(commits, N, "one commit per apply");
        assert_eq!(conflicts, 0, "sequential applies must not conflict");
        assert_eq!(final_val.0, N as i64, "no lost updates");
    }

    /// The bench's 8-worker shape through the real task layer: conflicts must happen, and no updates
    /// may be lost (the invariant a throughput number can never show).
    #[tokio::test(flavor = "multi_thread", worker_threads = 8)]
    async fn contention_probe_concurrent_increment_loses_nothing() {
        const N: usize = 200; // per worker
        const WORKERS: usize = 8;
        let mut vm = Vm::new(test_config());
        let code = indoc! { r#"
            int count = 0;

            void increment() {
                count = count + 1;
            }
        "# };
        let ctx = vm
            .initialize_string(code, "contention_probe_counter.c")
            .await
            .unwrap();
        let proc = ctx.process;
        let template = vm.new_task_template();

        let before = vm.committer_stats().await.unwrap();

        let mut set = tokio::task::JoinSet::new();
        for _ in 0..WORKERS {
            let proc = proc.clone();
            let template = template.clone();
            set.spawn(async move {
                for _ in 0..N {
                    apply_function_by_name("increment", &[], proc.clone(), template.clone(), None)
                        .await
                        .unwrap()
                        .unwrap();
                }
            });
        }
        while set.join_next().await.is_some() {}

        let after = vm.committer_stats().await.unwrap();
        let final_val = match vm.global_state.committed_global(&proc, 0) {
            LpcRef::Int(n) => n,
            other => panic!("counter not an int: {other:?}"),
        };

        let commits = after.commits - before.commits;
        let conflicts = after.conflicts - before.conflicts;
        let total = WORKERS * N;
        println!(
            "[probe:task-conc] commits={commits} conflicts={conflicts} final={final_val} (expect commits>={total}, conflicts>0, final={total})"
        );

        assert!(commits >= total, "every apply commits at least once");
        assert!(
            conflicts > 0,
            "8 concurrent global increments MUST conflict; 0 means contention is invisible"
        );
        assert_eq!(final_val.0, total as i64, "no lost updates");
    }

    /// The bench's `measure` pattern exactly — a fresh `multi_thread` runtime per worker count, N worker
    /// tasks of `apply_function_by_name` increments, a before/after `committer_stats` diff — plus the
    /// final committed counter (the lost-update check a throughput number can never show).
    #[test]
    fn contention_probe_measure_pattern_across_worker_counts() {
        const PER_WORKER: usize = 8192;
        for &workers in &[1usize, 4, 8] {
            let rt = tokio::runtime::Builder::new_multi_thread()
                .worker_threads(workers)
                .enable_all()
                .build()
                .unwrap();

            rt.block_on(async {
                let mut vm = Vm::new(test_config());
                let code = indoc! { r#"
                    int count = 0;

                    void increment() {
                        count = count + 1;
                    }
                "# };
                let ctx = vm
                    .initialize_string(code, "contention_probe_benchpat.c")
                    .await
                    .unwrap();
                let proc = ctx.process;
                let template = vm.new_task_template();

                let before = vm.committer_stats().await.unwrap();

                let mut set = tokio::task::JoinSet::new();
                for _ in 0..workers {
                    let proc = proc.clone();
                    let template = template.clone();
                    set.spawn(async move {
                        for _ in 0..PER_WORKER {
                            apply_function_by_name(
                                "increment",
                                &[],
                                proc.clone(),
                                template.clone(),
                                None,
                            )
                            .await
                            .unwrap()
                            .unwrap();
                        }
                    });
                }
                while set.join_next().await.is_some() {}

                let after = vm.committer_stats().await.unwrap();
                let final_val = match vm.global_state.committed_global(&proc, 0) {
                    LpcRef::Int(n) => n,
                    other => panic!("counter not an int: {other:?}"),
                };

                let commits = after.commits.saturating_sub(before.commits);
                let conflicts = after.conflicts.saturating_sub(before.conflicts);
                let total = workers * PER_WORKER;
                let rate = if commits > 0 {
                    conflicts as f64 / commits as f64
                } else {
                    0.0
                };
                println!(
                    "[probe:benchpat/{workers}] commits={commits} conflicts={conflicts} rate={rate:.4} final={0} (total_work={total})",
                    final_val.0
                );

                // The invariant the bench never checked: no lost updates.
                assert_eq!(
                    final_val.0,
                    total as i64,
                    "workers={workers}: counter must equal total work, not {total}"
                );
                // A single worker is sequential: zero conflicts expected.
                if workers == 1 {
                    assert_eq!(
                        conflicts, 0,
                        "1 worker is sequential; conflicts must be 0"
                    );
                }
            });
        }
    }
}
