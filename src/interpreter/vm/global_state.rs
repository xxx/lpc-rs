use std::{sync::Arc, thread::JoinHandle, time::Duration};

use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_function_support::program_function::ProgramFunction;
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;
use tracing::{instrument, trace};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        call_outs::CallOuts,
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
        lpc_ref::LpcRef,
        object_space::ObjectSpace,
        process::Process,
        stm::{
            self, CommitProtocol, CommittedReader, Committer, CommitterStats, GcPassReply,
            Snapshot, WorldRoot, gc_pass, resolve_or_create_object,
        },
        task::{Task, apply_function::apply_runtime_error, task_template::TaskTemplate},
        task_context::{TaskContext, confine_object_path},
        vm::vm_op::VmOp,
    },
};

/// A stored function pointer resolved and ready to run: the context carries
/// the receiver, the command giver, and the pointer's captures.
pub struct PreparedCall {
    pub context: TaskContext,
    pub function: Arc<ProgramFunction>,
    pub args: Vec<LpcRef>,
}

/// A type for globally-shared state that every [`Task`] will need access to.
#[derive(Debug)]
#[readonly::make]
pub struct GlobalState {
    /// Our object space, which stores all of the system objects (masters and clones)
    pub object_space: Arc<ObjectSpace>,

    /// The [`Config`] that's in use for this [`Vm`](crate::interpreter::vm::Vm)
    pub config: Arc<Config>,

    /// Enqueued call outs
    call_outs: RwLock<CallOuts>,

    /// The channel used to send [`VmOp`]s to the [`Vm`](crate::interpreter::vm::Vm)
    pub tx: Sender<VmOp>,

    /// Sender to this state's single committer thread.
    pub(crate) committer_tx: flume::Sender<CommitProtocol>,

    /// Handle to the committer thread.
    committer_handle: Option<JoinHandle<Snapshot>>,

    /// Attempt-loop totals, recorded by `run_attempts`.
    pub(crate) attempt_telemetry: Arc<stm::AttemptTelemetry>,

    /// Subscription to the committer's per-commit watermark.
    pub(crate) commit_watch: tokio::sync::watch::Receiver<stm::Version>,

    /// When this state was built; MSSP's `UPTIME`.
    pub booted_at: std::time::SystemTime,

    /// Every live connection; the loop registers and removes itself.
    pub registry: crate::interpreter::vm::binding::Registry,
}

impl GlobalState {
    pub fn new<C>(config: C, tx: Sender<VmOp>) -> Self
    where
        C: Into<Arc<Config>>,
    {
        let conf = config.into();
        let (committer_tx, commit_watch, committer_handle) = Self::spawn_committer();

        Self {
            object_space: Arc::new(ObjectSpace::new(conf.clone())),
            config: conf,
            call_outs: RwLock::new(CallOuts::new(tx.clone())),
            tx,
            committer_tx,
            committer_handle: Some(committer_handle),
            attempt_telemetry: Arc::default(),
            commit_watch,
            booted_at: std::time::SystemTime::now(),
            registry: Default::default(),
        }
    }

    /// Spawn a committer thread; return its sender, watermark subscription,
    /// and join handle.
    fn spawn_committer() -> (
        flume::Sender<CommitProtocol>,
        tokio::sync::watch::Receiver<stm::Version>,
        JoinHandle<Snapshot>,
    ) {
        let (tx, rx) = flume::unbounded();
        let committer_tx = tx.clone();
        let committer = Committer::new();
        let commit_watch = committer.commit_watch();
        let handle = std::thread::Builder::new()
            .name("stm-committer".into())
            .spawn(move || committer.run(committer_tx, rx))
            .expect("failed to spawn committer thread");
        (tx, commit_watch, handle)
    }

    /// A state whose committer rejects its first `rejections` commits.
    #[cfg(test)]
    pub(crate) fn new_rejecting(config: Arc<Config>, tx: Sender<VmOp>, rejections: usize) -> Self {
        let mut state = Self::new(config, tx);
        state.close_committer();
        if let Some(handle) = state.committer_handle.take() {
            let _ = handle.join();
        }
        let (committer_tx, rx) = flume::unbounded();
        let loop_tx = committer_tx.clone();
        let committer = Committer::new();
        state.commit_watch = committer.commit_watch();
        let handle =
            std::thread::spawn(move || committer.run_with_rejections(loop_tx, rx, rejections));
        state.committer_tx = committer_tx;
        state.committer_handle = Some(handle);
        state
    }

    /// Tell the committer to shut down. Idempotent: the committer exits on
    /// the first `Close`; later sends just fail (channel closed).
    pub fn close_committer(&self) {
        let _ = self.committer_tx.send(CommitProtocol::Close);
    }

    /// The committer's lifetime commit totals (commits/conflicts/errors),
    /// for benches and tooling.
    pub async fn committer_stats(&self) -> Result<CommitterStats> {
        stm::committer_stats(&self.committer_tx).await
    }

    /// The committer channel's current backlog.
    pub fn committer_queue_len(&self) -> usize {
        self.committer_tx.len()
    }

    /// The attempt loop's lifetime totals; for bench measurement and
    /// tooling, not the hot path.
    pub fn attempt_telemetry(&self) -> stm::AttemptTelemetrySnapshot {
        self.attempt_telemetry.snapshot()
    }

    /// Resolve `ptr` for a call started outside any task (`call_out`,
    /// `input_to`) with `passed` arguments, initializing the receiver if needed
    /// with `this_player` as the command giver.
    /// `Ok(None)`: the receiver has no such function, or its initializer
    /// failed and the error was already reported.
    pub async fn prepare_function_ptr(
        self: &Arc<Self>,
        ptr: &FunctionPtr,
        passed: &[LpcRef],
        this_player: Option<Arc<Process>>,
    ) -> Result<Option<PreparedCall>> {
        // The transactional seam: a create-on-miss goes through the
        // committer, and a destruct in the committed-unflushed window is an
        // error instead of a resurrection.
        self.resolve_dynamic_string_receiver(ptr).await?;

        // The context only needs a live object to sit in until the receiver is known.
        let seat = ptr.owner.upgrade().or_else(|| match &ptr.address {
            FunctionAddress::Local(receiver, _) => receiver.upgrade(),
            _ => None,
        });
        let Some(seat) = seat else {
            return Err(lpc_error!(
                "attempted to call a function pointer whose owner is destructed: {}",
                ptr
            ));
        };
        let template = TaskTemplate::from(self.clone());
        template.set_this_player(this_player.clone());
        let Some(resolved) = ptr
            .prepare_call(passed, &template.into_task_context(seat))
            .await?
        else {
            return Ok(None);
        };

        let process = &resolved.process;
        if !self.is_initialized(process) {
            let template = TaskTemplate::from(self.clone());
            template.set_this_player(this_player.clone());
            let ctx = template.into_task_context(process.clone());
            if let Err(e) = Task::<MAX_CALL_STACK_SIZE>::initialize_process(ctx).await {
                let template = TaskTemplate::from(self.clone());
                if !matches!(
                    apply_runtime_error(&e, Some(process.clone()), template).await,
                    Some(Ok(_))
                ) {
                    self.config.debug_log(e.diagnostic_string()).await;
                }
                return Ok(None);
            }
        }

        let mut template = TaskTemplate::from(self.clone());
        template.set_this_player(this_player);
        template.upvalue_ptrs = Some(ptr.upvalue_ptrs.clone());
        Ok(Some(PreparedCall {
            context: template.into_task_context(process.clone()),
            function: resolved.function,
            args: resolved.args,
        }))
    }

    /// Resolve `ptr`'s receiver for a call started outside any task
    /// (`call_out`, `input_to`), through `valid_load` for the pointer's writer.
    /// A create-on-miss goes through the committer, and a destruct in the
    /// committed-unflushed window reads as a miss, so a fresh object is
    /// created rather than the destructed one handed back.
    ///
    /// Returns `Ok(None)` when `ptr` is not a dynamic string receiver (a
    /// different address kind, or a missing / non-string first partial arg);
    /// the caller falls through to [`FunctionPtr::prepare_call`].
    pub async fn resolve_dynamic_string_receiver(
        self: &Arc<Self>,
        ptr: &FunctionPtr,
    ) -> Result<Option<Arc<Process>>> {
        let FunctionAddress::Dynamic(_) = &ptr.address else {
            return Ok(None);
        };
        let Some(first) = ptr.partial_args().first().cloned().flatten() else {
            return Ok(None);
        };
        let LpcRef::String(_) = &first else {
            return Ok(None);
        };
        let path = first
            .with_string(|s| confine_object_path(&self.config, s.to_str(), "/", "call_other"))??;
        let loader = ptr.loader(self.config.lib_dir.as_str())?;
        Ok(Some(resolve_or_create_object(self, &path, &loader).await?))
    }

    /// One GC pass, atomic on the committer: mark the world from the live
    /// objects and the queued call-outs, then reclaim every var the walk did
    /// not reach.
    ///
    /// # Precondition
    ///
    /// The committer must be **quiescent** — a non-quiescent call is refused
    /// (`Ok(Err(_))`) rather than blocked: a concurrent task at a GC point is
    /// a caller bug.
    #[instrument(skip_all)]
    pub async fn gc(&self) -> Result<GcPassReply> {
        let mut roots: Vec<WorldRoot> = self
            .object_space
            .all_cell_ids()
            .into_iter()
            .map(WorldRoot::Var)
            .collect();
        // Bootstrap objects have no committed cell, so their global slots are
        // rooted directly: `all_cell_ids` alone would wrongly reclaim them.
        self.object_space
            .all_live_object_slots()
            .into_iter()
            .for_each(|id| roots.push(WorldRoot::Var(id)));
        self.with_call_outs(|co| {
            for (_, call_out) in co.queue() {
                roots.push(WorldRoot::Ref(call_out.func_ref.clone()));
            }
        });

        gc_pass(&self.committer_tx, roots).await
    }

    /// Run a GC pass at the next quiet moment: a refused pass (attempts in
    /// flight) is retried up to `retries` times, `delay` apart, then the
    /// committer's refusal is returned. A failed pass is not retried.
    pub async fn gc_when_quiet(&self, retries: u32, delay: Duration) -> Result<GcPassReply> {
        let mut attempt = 0;
        loop {
            match self.gc().await? {
                Err(refused) if attempt < retries => {
                    trace!("gc pass deferred: {refused}");
                    attempt += 1;
                    tokio::time::sleep(delay).await;
                }
                outcome => return Ok(outcome),
            }
        }
    }

    pub fn with_call_outs<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&CallOuts) -> R,
    {
        f(&self.call_outs.read())
    }

    /// The call-out queue's lock, for flushing deferred scheduling effects
    /// (the flush needs a `&RwLock`, not a closure, because it spans awaits).
    pub(crate) fn call_outs(&self) -> &RwLock<CallOuts> {
        &self.call_outs
    }

    pub fn with_call_outs_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut CallOuts) -> R,
    {
        f(&mut self.call_outs.write())
    }
}

impl Drop for GlobalState {
    fn drop(&mut self) {
        // Send `Close` so the committer exits even if our sender were the
        // last one (it isn't — the committer holds a clone — but `Close`
        // makes the exit deterministic rather than relying on channel
        // close), then join so we never leak the thread. `let _ =` on the
        // join: a panicked committer (which shouldn't happen — `run`/
        // `process` don't panic) must not take the drop of the whole
        // interpreter state with it.
        self.close_committer();
        if let Some(handle) = self.committer_handle.take() {
            let _ = handle.join();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::{
        interpreter::{
            function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
            lpc_ref::LpcRef,
            object_space::ObjectSpace,
            process::Process,
            stm::{
                CommittedReader, Transaction, TxnHandle, commit_changeset, flush_effects, start_txn,
            },
        },
        test_support::{permissive_master, test_config},
        util::process_builder::{compile_process_from_code, process_insert_and_initialize_program},
    };
    use lpc_rs_core::lpc_path::LpcPath;
    use thin_vec::thin_vec;
    use ustr::ustr;

    #[test]
    fn committer_thread_exits_when_global_state_drops() {
        let (tx, _rx) = tokio::sync::mpsc::channel(8);
        let probe;
        {
            let gs = Arc::new(GlobalState::new(test_config(), tx));
            probe = gs.committer_tx.clone();
        } // last Arc released here -> GlobalState::drop sends Close + joins.
        // If the join ever blocked, this test would hang.
        assert!(
            probe.is_disconnected(),
            "committer channel should be closed after the committer exited"
        );
    }

    /// A `GlobalState` with a live committer and the in-game test config
    /// (whose code root is `tests/fixtures/code`).
    fn state_for_receiver() -> Arc<GlobalState> {
        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        Arc::new(GlobalState::new(test_config(), tx))
    }

    /// A `&->foo(path)` pointer owned by `owner`.
    fn string_receiver_ptr(path: &str, owner: &Arc<Process>) -> FunctionPtr {
        FunctionPtrBuilder::default()
            .address(FunctionAddress::Dynamic(ustr("foo")))
            .owner(Arc::downgrade(owner))
            .partial_args(thin_vec![Some(LpcRef::String(Arc::new(
                path.to_string().into()
            )))])
            .build()
            .unwrap()
    }

    /// Commit one object cell through the committer protocol, so the cell is
    /// visible to a later `resolve_or_create_object`. (A raw
    /// `Changeset::new(Version::new())` would be rejected: it is seeded at a
    /// version ahead of the committer's current one.)
    async fn commit_object_cell(
        gs: &GlobalState,
        process: Arc<Process>,
    ) -> std::result::Result<(), lpc_rs_errors::LpcError> {
        let key = gs
            .object_space
            .path_key(LpcPath::InGame(std::path::PathBuf::from("/dynamic_receiver")).as_ref());
        let cell_id = gs.object_space.cell_id(&key);
        let mut live = start_txn(&gs.committer_tx).await?;
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        txn.with(|t| t.write_process(cell_id, process));
        let changeset = txn.with(|t| t.take_changeset());
        live.disarm(); // the commit carries the release
        let commit = commit_changeset(&gs.committer_tx, changeset).await?;
        let effects = txn.with(|t| t.take_effects());
        drop(live);
        commit.expect("object cell commit must succeed");
        if !effects.is_empty() {
            flush_effects(gs, effects).await;
        }
        Ok(())
    }

    /// The committed cell is the single source of truth: a receiver whose
    /// cell is already committed resolves to that exact `Arc`.
    #[tokio::test]
    async fn string_receiver_finds_committed() {
        let gs = state_for_receiver();
        let master = permissive_master(&gs.object_space).await;
        let process = Arc::new(Process::new(
            crate::interpreter::program::ProgramBuilder::default()
                .filename(LpcPath::InGame(std::path::PathBuf::from(
                    "/dynamic_receiver",
                )))
                .build()
                .unwrap(),
        ));
        commit_object_cell(&gs, process.clone()).await.unwrap();

        let got = gs
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver", &master))
            .await
            .unwrap()
            .expect("a committed string receiver should resolve");
        assert!(
            Arc::ptr_eq(&got, &process),
            "should hand back the committed process"
        );
    }

    /// An object in the physical map but with no committed cell (a bootstrap
    /// object, created outside a transaction) resolves to that same `Arc`.
    #[tokio::test]
    async fn string_receiver_finds_physical_only() {
        let gs = state_for_receiver();
        let master = permissive_master(&gs.object_space).await;
        let process = Arc::new(Process::new(
            crate::interpreter::program::ProgramBuilder::default()
                .filename(LpcPath::InGame(std::path::PathBuf::from(
                    "/dynamic_receiver",
                )))
                .build()
                .unwrap(),
        ));
        ObjectSpace::insert_process_physical(&gs.object_space, process.clone());

        let got = gs
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver", &master))
            .await
            .unwrap()
            .expect("a physical-only string receiver should resolve");
        assert!(
            Arc::ptr_eq(&got, &process),
            "should hand back the physical process"
        );
    }

    /// A committed cell that reads back absent while the physical object is
    /// still present (a committed, not-yet-flushed destruct) is a miss: a
    /// fresh object is created, never the destructed one handed back.
    #[tokio::test]
    async fn string_receiver_unflushed_destruct_creates_fresh() {
        let gs = state_for_receiver();
        let master = permissive_master(&gs.object_space).await;
        let process = Arc::new(Process::new(
            crate::interpreter::program::ProgramBuilder::default()
                .filename(LpcPath::InGame(std::path::PathBuf::from(
                    "/dynamic_receiver",
                )))
                .build()
                .unwrap(),
        ));
        let key = gs.object_space.path_key("/dynamic_receiver");
        let cell_id = gs.object_space.cell_id(&key);

        commit_object_cell(&gs, process.clone()).await.unwrap();
        ObjectSpace::insert_process_physical(&gs.object_space, process.clone());

        // Commit a destruct: drop the cell, but never flush the physical
        // removal, leaving the destruct window open.
        let mut live = start_txn(&gs.committer_tx).await.unwrap();
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        txn.with(|t| t.drop_var(cell_id));
        let changeset = txn.with(|t| t.take_changeset());
        live.disarm(); // the commit carries the release
        let commit = commit_changeset(&gs.committer_tx, changeset).await.unwrap();
        drop(live);
        commit.expect("destruct commit must succeed");

        assert!(
            gs.committed_object(cell_id).is_none(),
            "cell must read back absent"
        );
        assert!(
            gs.object_space.lookup(&key).is_some(),
            "physical entry still present"
        );

        let fresh = gs
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver", &master))
            .await
            .unwrap()
            .expect("a destructed path is a miss");
        assert!(
            !Arc::ptr_eq(&fresh, &process),
            "the destructed object must not be handed out"
        );
        let committed = gs.committed_object(cell_id).expect("cell committed");
        let physical = gs.object_space.lookup(&key).expect("physically present");
        assert!(Arc::ptr_eq(&committed, &fresh));
        assert!(Arc::ptr_eq(&physical, &fresh));
    }

    /// A true miss compiles the file, commits the cell, and flushes the
    /// physical insert, so the receiver is usable (and not pre-initialized)
    /// when this returns.
    #[tokio::test]
    async fn string_receiver_miss_creates_transactionally() {
        let gs = state_for_receiver();
        let master = permissive_master(&gs.object_space).await;

        let got = gs
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver", &master))
            .await
            .unwrap()
            .expect("a miss should create the receiver");

        let key = gs.object_space.process_key(&got);
        assert!(
            gs.object_space.lookup(&key).is_some(),
            "created receiver must be physically present"
        );
        let cell_id = gs
            .object_space
            .get_cell_id(&key)
            .expect("cell must be minted");
        assert!(
            gs.committed_object(cell_id).is_some(),
            "created receiver's cell must be committed"
        );
        assert!(
            !gs.is_initialized(&got),
            "a create-on-miss leaves the receiver uninitialized"
        );
    }

    /// Two concurrent creates of the same unknown path both succeed and the
    /// committed cell holds the receiver: the create is serialized through the
    /// committer on a single cell for the path.
    #[tokio::test]
    async fn string_receiver_concurrent_creates_converge() {
        let gs = state_for_receiver();
        let master = permissive_master(&gs.object_space).await;

        let ptr = string_receiver_ptr("/dynamic_receiver", &master);
        let (ra, rb) = tokio::join!(
            gs.resolve_dynamic_string_receiver(&ptr),
            gs.resolve_dynamic_string_receiver(&ptr)
        );
        let a = ra.unwrap().expect("concurrent create A should succeed");
        let b = rb.unwrap().expect("concurrent create B should succeed");

        let key = gs.object_space.process_key(&a);
        assert_eq!(
            key,
            gs.object_space.process_key(&b),
            "both must target the same path"
        );
        assert!(Arc::ptr_eq(&a, &b), "both callers must get the one object");
        let cell_id = gs.object_space.get_cell_id(&key).expect("cell must exist");
        let committed = gs
            .committed_object(cell_id)
            .expect("cell must hold a committed process");
        let physical = gs
            .object_space
            .lookup(&key)
            .expect("receiver must be physically present");
        assert!(Arc::ptr_eq(&committed, &a));
        assert!(Arc::ptr_eq(&physical, &a));
    }

    /// A rejected create re-runs: the second round finds the committed cell
    /// or creates again, and the one object it returns is the committed and
    /// physical one.
    #[tokio::test]
    async fn string_receiver_create_survives_a_rejected_commit() {
        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let gs = Arc::new(GlobalState::new_rejecting(Arc::new(test_config()), tx, 1));
        let master = permissive_master(&gs.object_space).await;

        let got = gs
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver", &master))
            .await
            .unwrap()
            .expect("a miss should create the receiver");

        let key = gs.object_space.process_key(&got);
        let cell_id = gs.object_space.get_cell_id(&key).expect("cell must exist");
        let committed = gs.committed_object(cell_id).expect("cell committed");
        let physical = gs.object_space.lookup(&key).expect("physically present");
        assert!(Arc::ptr_eq(&committed, &got));
        assert!(Arc::ptr_eq(&physical, &got));
        assert_eq!(
            gs.committer_stats().await.unwrap().commits,
            1,
            "one round was rejected synthetically, the next committed"
        );
    }

    /// A find hit has nothing to commit: the second resolve of a path adds
    /// no commit.
    #[tokio::test]
    async fn string_receiver_find_hit_commits_nothing() {
        let gs = state_for_receiver();
        let master = permissive_master(&gs.object_space).await;
        let ptr = string_receiver_ptr("/dynamic_receiver", &master);

        let created = gs
            .resolve_dynamic_string_receiver(&ptr)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(gs.committer_stats().await.unwrap().commits, 1);

        let found = gs
            .resolve_dynamic_string_receiver(&ptr)
            .await
            .unwrap()
            .unwrap();
        assert!(Arc::ptr_eq(&created, &found));
        assert_eq!(gs.committer_stats().await.unwrap().commits, 1);
    }

    /// A non-dynamic address, a missing first partial arg, or a non-string
    /// first arg all return `Ok(None)`: the caller falls through to `triple`.
    #[tokio::test]
    async fn string_receiver_non_dynamic_or_non_string_returns_none() {
        let gs = state_for_receiver();

        let efun = FunctionPtrBuilder::default()
            .address(FunctionAddress::Efun(ustr("find_object")))
            .build()
            .unwrap();
        assert!(
            gs.resolve_dynamic_string_receiver(&efun)
                .await
                .unwrap()
                .is_none()
        );

        let no_args = FunctionPtrBuilder::default()
            .address(FunctionAddress::Dynamic(ustr("foo")))
            .build()
            .unwrap();
        assert!(
            gs.resolve_dynamic_string_receiver(&no_args)
                .await
                .unwrap()
                .is_none()
        );

        let dummy = Arc::new(Process::new(
            crate::interpreter::program::ProgramBuilder::default()
                .build()
                .unwrap(),
        ));
        let obj_arg = FunctionPtrBuilder::default()
            .address(FunctionAddress::Dynamic(ustr("foo")))
            .partial_args(thin_vec![Some(LpcRef::Object(Arc::downgrade(&dummy)))])
            .build()
            .unwrap();
        assert!(
            gs.resolve_dynamic_string_receiver(&obj_arg)
                .await
                .unwrap()
                .is_none()
        );
    }

    /// A rejected parent attempt that lazily initialized a callee re-runs
    /// and initializes it again, rather than leaving its globals null.
    #[tokio::test]
    async fn lazy_callee_init_survives_a_rejected_commit() {
        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let gs = Arc::new(GlobalState::new_rejecting(Arc::new(test_config()), tx, 1));

        let (target, _) = compile_process_from_code(
            &gs.object_space,
            "/target.c",
            "int i = 123; int get() { return i; }",
            None,
        )
        .await
        .unwrap();
        ObjectSpace::insert_process_physical(&gs.object_space, target.clone());
        assert!(!gs.is_initialized(&target));

        let (caller, _) = compile_process_from_code(
            &gs.object_space,
            "/caller.c",
            r#"int r = "/target"->get();"#,
            None,
        )
        .await
        .unwrap();
        process_insert_and_initialize_program::<MAX_CALL_STACK_SIZE>(
            caller.clone(),
            TaskTemplate::from(gs.clone()),
        )
        .await
        .unwrap();

        assert_eq!(gs.committed_global(&target, 0), LpcRef::from(123));
        assert_eq!(gs.committed_global(&caller, 0), LpcRef::from(123));
        assert!(gs.is_initialized(&target));
        assert_eq!(gs.committer_stats().await.unwrap().commits, 1);
    }

    /// Two attempts claiming one initialization conflict at commit; the
    /// loser then finds the object initialized.
    #[tokio::test]
    async fn concurrent_init_claims_conflict_at_commit() {
        let (tx, _rx) = tokio::sync::mpsc::channel(8);
        let gs = Arc::new(GlobalState::new(test_config(), tx));
        let process = Arc::new(Process::default());

        let mut a = start_txn(&gs.committer_tx).await.unwrap();
        let mut b = start_txn(&gs.committer_tx).await.unwrap();
        let txn_a = TxnHandle::new(Transaction::new(a.inner.clone()));
        let txn_b = TxnHandle::new(Transaction::new(b.inner.clone()));
        assert!(process.claim_init(&txn_a));
        assert!(process.claim_init(&txn_b));
        assert!(
            !process.claim_init(&txn_a),
            "a second claim in one attempt is a no-op"
        );

        let changeset_a = txn_a.with(|t| t.take_changeset());
        let changeset_b = txn_b.with(|t| t.take_changeset());
        // the commits carry the releases
        a.disarm();
        b.disarm();
        assert!(
            commit_changeset(&gs.committer_tx, changeset_a)
                .await
                .unwrap()
                .is_ok()
        );
        assert!(
            commit_changeset(&gs.committer_tx, changeset_b)
                .await
                .unwrap()
                .is_err()
        );
        drop(a);
        drop(b);

        assert!(gs.is_initialized(&process));
        let c = start_txn(&gs.committer_tx).await.unwrap();
        let txn_c = TxnHandle::new(Transaction::new(c.inner.clone()));
        assert!(process.is_initialized(&txn_c));
        assert!(!process.claim_init(&txn_c));
    }
}
