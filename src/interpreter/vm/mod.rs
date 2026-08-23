use std::{sync::Arc, time::Duration};

use flume::Sender as FlumeSender;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use tokio::{signal, sync::mpsc::Receiver};
use tracing::{debug, error, info, instrument};
use vm_op::VmOp;

use crate::{
    compile_time_config::VM_CHANNEL_CAPACITY,
    interpreter::{
        SHUTDOWN,
        task::{apply_function::apply_function_in_master, task_template::TaskTemplate},
        task_context::TaskContext,
        vm::global_state::GlobalState,
    },
    telnet::{Telnet, connection_broker::ConnectionBroker, ops::BrokerOp},
};

mod initiate_login;
mod object_initializers;
mod prioritize_call_out;
mod takeover;

pub mod global_state;
pub mod vm_op;

#[derive(Debug)]
#[readonly::make]
pub struct Vm {
    pub global_state: Arc<GlobalState>,

    /// The connection broker, which handles all of the network connections
    connection_broker: ConnectionBroker,

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

        let config = &self.global_state.config;
        let address = format!("{}:{}", config.bind_address, config.port);
        self.connection_broker
            .run(address, TaskTemplate::from(self.global_state.clone()))
            .await;
        self.run().await
    }

    /// Load and initialize the master object and simul_efuns.
    ///
    /// # Returns
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the master object
    /// * `Err(LpcError)` - If there was an error.
    pub async fn bootstrap(&mut self) -> Result<TaskContext> {
        if let Some(Err(e)) = self.global_state.initialize_simul_efuns().await {
            e.emit_diagnostics();
            return Err(e);
        }

        let config = &self.global_state.config;
        let master_path = LpcPath::new_in_game(&*config.master_object, "/", &*config.lib_dir);
        self.global_state
            .initialize_process_from_path(&master_path)
            .await
            .map(|t| t.context)
    }

    /// Run the [`Vm`]'s main loop, which is the main event loop for the entire system.
    /// Assumes `bootstrap()` has already been called.
    /// This runs on the main execution thread, and should never do any work itself.
    /// Spawn a task to do anything beyond message handling, or logging.
    #[instrument(skip_all)]
    pub async fn run(&mut self) -> lpc_rs_errors::Result<()> {
        let gc_interval = self.global_state.config.gc_interval;
        // `interval` panics on a zero period; a disabled collector is gated off at the arm.
        let mut gc_ticks = tokio::time::interval(Duration::from_secs(gc_interval.max(1)));
        gc_ticks.tick().await;

        loop {
            tokio::select! {
                biased; // we want signal handlers checked first, always.
                _ = signal::ctrl_c() => {
                    // SIGINT on Linux
                    info!("Ctrl-C received... shutting down");
                    break;
                }
                _ = gc_ticks.tick(), if gc_interval > 0 => {
                    let global_state = self.global_state.clone();
                    tokio::spawn(async move {
                        match global_state.gc_when_quiet(20, Duration::from_millis(100)).await {
                            Ok(report) => info!("gc pass reclaimed {} vars", report.reclaimed),
                            Err(e) => debug!("gc pass skipped this period: {e}"),
                        }
                    });
                }
                Some(op) = self.rx.recv() => {
                    match op {
                        VmOp::InitiateLogin(connection) => {
                            self.initiate_login(connection).await;
                        }
                        VmOp::PrioritizeCallOut(id) => {
                            self.global_state.prioritize_call_out(id).await;
                        }
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
            TaskTemplate::from(self.global_state.clone()),
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
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader, lpc_ref::LpcRef, process::Process, stm::start_txn,
            task::apply_function::apply_function_by_name,
        },
        test_support::test_config,
    };

    /// Closures held only by another object's committed global keep their
    /// captured cells across a pass; the cells go once nothing holds them.
    #[tokio::test]
    async fn test_gc() {
        let vm = Vm::new(test_config());
        let storage = indoc! { r#"
            function *storage = ({});
            int total;

            void store(function f) {
                storage += ({ f });
            }

            void runem() {
                foreach (f: storage) {
                    total += f();
                }
            }
        "# };

        let runner = indoc! { r#"
            void create() {
                int i = -1;

                object storage = clone_object("/storage");

                while(++i < 5) {
                    storage->store((: i :));
                }
            }
        "# };

        let ctx1 = vm.initialize_string(storage, "storage").await.unwrap();
        let _ctx2 = vm.initialize_string(runner, "runner").await.unwrap();
        let storage_proc = vm.global_state.object_space.lookup("/storage#0").unwrap();
        let template = TaskTemplate::from(ctx1.global_state.clone());

        // The only garbage is the arrays `+=` left behind; the capture cell stays.
        vm.global_state.gc().await.unwrap();

        apply_function_by_name("runem", &[], storage_proc.clone(), template.clone(), None)
            .await
            .unwrap()
            .unwrap();
        // Five closures over one cell, which the loop left at 5.
        assert_eq!(
            vm.global_state.committed_global(&storage_proc, 1),
            LpcRef::from(25)
        );

        vm.global_state.object_space.clear();

        let report = vm.global_state.gc().await.unwrap();
        assert!(
            report.reclaimed > 0,
            "unreachable objects' cells are reclaimed"
        );
    }

    /// The cell of a closure that died with its frame is reclaimed.
    #[tokio::test]
    async fn gc_reclaims_the_cells_of_a_popped_frame() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            void create() { int j = 1; function f = (: j :); }
        "# };
        vm.initialize_string(code, "popped").await.unwrap();

        let report = vm.global_state.gc().await.unwrap();
        assert_eq!(report.reclaimed, 1);
    }

    /// A destructed object's committed globals are reclaimed.
    #[tokio::test]
    async fn gc_reclaims_a_destructed_objects_globals() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            void create() {
                object o = clone_object("/ptr_target");
                destruct(o);
            }
        "# };
        vm.initialize_string(code, "destructor").await.unwrap();

        let report = vm.global_state.gc().await.unwrap();
        assert!(report.reclaimed >= 1, "{report:?}");
    }

    mod gc_when_quiet {
        use std::time::Duration;

        use super::*;

        #[tokio::test]
        async fn runs_at_once_when_nothing_is_in_flight() {
            let vm = Vm::new(test_config());
            vm.global_state
                .gc_when_quiet(3, Duration::from_millis(10))
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn waits_for_a_transaction_to_finish() {
            let vm = Vm::new(test_config());
            let tx = vm.global_state.committer_tx.clone();
            let live = start_txn(&tx).await.unwrap();

            let releaser = tokio::spawn(async move {
                tokio::time::sleep(Duration::from_millis(50)).await;
                drop(live);
            });
            vm.global_state
                .gc_when_quiet(20, Duration::from_millis(10))
                .await
                .unwrap();
            releaser.await.unwrap();
        }

        #[tokio::test]
        async fn gives_up_after_its_retries() {
            let vm = Vm::new(test_config());
            let tx = vm.global_state.committer_tx.clone();
            let _live = start_txn(&tx).await.unwrap();

            let err = vm
                .global_state
                .gc_when_quiet(3, Duration::from_millis(5))
                .await
                .unwrap_err();
            assert!(err.to_string().contains("not quiescent"), "{err}");
        }
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
        let vm = Vm::new(test_config());
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
        let template = TaskTemplate::from(ctx.global_state.clone());

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
        let vm = Vm::new(test_config());

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
        let vm = Vm::new(test_config());
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
        let template = TaskTemplate::from(vm.global_state.clone());

        let before = vm.global_state.committer_stats().await.unwrap();
        for _ in 0..N {
            apply_function_by_name("increment", &[], proc.clone(), template.clone(), None)
                .await
                .unwrap()
                .unwrap();
        }
        let after = vm.global_state.committer_stats().await.unwrap();
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
        let vm = Vm::new(test_config());
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
        let template = TaskTemplate::from(vm.global_state.clone());

        let before = vm.global_state.committer_stats().await.unwrap();

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

        let after = vm.global_state.committer_stats().await.unwrap();
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
                let vm = Vm::new(test_config());
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
                let template = TaskTemplate::from(vm.global_state.clone());

                let before = vm.global_state.committer_stats().await.unwrap();

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

                let after = vm.global_state.committer_stats().await.unwrap();
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
