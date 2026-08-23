use std::{path::PathBuf, sync::Arc, thread::JoinHandle};

use bit_set::BitSet;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;
use tracing::instrument;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        call_outs::CallOuts,
        function_type::{
            function_address::FunctionAddress,
            function_ptr::{FunctionPtr, PtrTriple},
        },
        gc::{gc_bank::GcVarIdBank, mark::Mark},
        lpc_ref::LpcRef,
        object_space::ObjectSpace,
        process::Process,
        stm::{
            self, CommitProtocol, CommittedReader, Committer, CommitterStats, GcReport, Snapshot,
            VarId, WorldRoot, gc_pass, resolve_or_create_object,
        },
        task::{Task, apply_function::apply_runtime_error, task_template::TaskTemplate},
        vm::vm_op::VmOp,
    },
};

/// A type for globally-shared state that every [`Task`] will need access to.
#[derive(Debug)]
#[readonly::make]
pub struct GlobalState {
    /// Our object space, which stores all of the system objects (masters and clones)
    pub object_space: Arc<ObjectSpace>,

    /// All upvalues are stored in the [`Vm`](crate::interpreter::vm::Vm), and are shared between all [`Task`]s.
    /// Each slot holds a transactional `VarId`; the committed value lives in the committer's world.
    upvalues: Arc<RwLock<GcVarIdBank>>,

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
}

impl GlobalState {
    pub fn new<C>(config: C, tx: Sender<VmOp>) -> Self
    where
        C: Into<Arc<Config>>,
    {
        let conf = config.into();
        let (committer_tx, committer_handle) = Self::spawn_committer();

        Self {
            object_space: Arc::new(ObjectSpace::new(conf.clone())),
            upvalues: Arc::new(RwLock::new(GcVarIdBank::default())),
            config: conf,
            call_outs: RwLock::new(CallOuts::new(tx.clone())),
            tx,
            committer_tx,
            committer_handle: Some(committer_handle),
        }
    }

    /// Spawn a committer thread; return its sender and join handle.
    fn spawn_committer() -> (flume::Sender<CommitProtocol>, JoinHandle<Snapshot>) {
        let (tx, rx) = flume::unbounded();
        let committer_tx = tx.clone();
        let handle = std::thread::Builder::new()
            .name("stm-committer".into())
            .spawn(move || Committer::new().run(committer_tx, rx))
            .expect("failed to spawn committer thread");
        (tx, handle)
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
        let handle = std::thread::spawn(move || {
            Committer::new().run_with_rejections(loop_tx, rx, rejections)
        });
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

    /// Resolve `ptr` to its process, function and partial args for a call
    /// started outside any task (`call_out`, `input_to`), initializing the
    /// receiver if needed with `this_player` as the command giver.
    /// `Ok(None)`: the initializer failed and the error was already reported.
    pub async fn prepare_function_ptr(
        self: &Arc<Self>,
        ptr: &FunctionPtr,
        this_player: Option<Arc<Process>>,
    ) -> Result<Option<PtrTriple>> {
        // The transactional seam: a create-on-miss goes through the
        // committer, and a destruct in the committed-unflushed window is an
        // error instead of a resurrection.
        self.resolve_dynamic_string_receiver(ptr).await?;
        let (process, function, args) =
            FunctionPtr::triple(ptr, &self.config, &self.object_space).await?;

        if !self.is_initialized(&process) {
            let template = TaskTemplate::from(self.clone());
            template.set_this_player(this_player);
            let ctx = template.into_task_context(process.clone());
            if let Err(e) = Task::<MAX_CALL_STACK_SIZE>::initialize_process(ctx).await {
                let template = TaskTemplate::from(self.clone());
                if !matches!(
                    apply_runtime_error(&e, Some(process), template).await,
                    Some(Ok(_))
                ) {
                    self.config.debug_log(e.diagnostic_string()).await;
                }
                return Ok(None);
            }
        }

        Ok(Some((process, function, args)))
    }

    /// Resolve the receiver of a dynamic function pointer whose first partial
    /// argument is an object path as a string (`call_out` / `input_to` targets).
    ///
    /// Runs the cell-first find + create-on-miss in its own short-lived
    /// transaction (committed before this returns). A committed-but-unflushed
    /// destruct is an error, not a resurrection.
    ///
    /// Returns `Ok(None)` when `ptr` is not a dynamic string receiver (a
    /// different address kind, or a missing / non-string first partial arg);
    /// the caller falls through to [`FunctionPtr::triple`].
    pub async fn resolve_dynamic_string_receiver(
        &self,
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
        let path_buf = first.with_string(|s| PathBuf::from(s.to_str()))?;
        let path = LpcPath::InGame(path_buf);
        Ok(Some(resolve_or_create_object(self, &path).await?))
    }

    /// One GC pass, atomic on the committer: refuse unless quiescent, else
    /// cull the unmarked upvalue cells and reclaim the unreachable world
    /// payload vars. The cells travel inside the pass message, so their drops
    /// cannot be reordered past the sweep.
    ///
    /// # Precondition
    ///
    /// The committer must be **quiescent** — a non-quiescent call is refused
    /// rather than blocked: a concurrent task at a GC point is a caller bug.
    /// A refused pass leaves the bank untouched.
    #[instrument(skip_all)]
    pub async fn gc(&self) -> Result<GcReport> {
        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        self.mark(&mut marked, &mut processed)?;

        // The culled cells' ids, computed without mutating the bank; the
        // surviving cells are the world sweep's upvalue roots. The read guard
        // is scoped to this block so it drops before the pass's await.
        let (dropped, survivors): (Vec<VarId>, Vec<VarId>) = {
            let bank = self.upvalues.read();
            (
                bank.iter()
                    .filter(|(idx, _)| !marked.contains(*idx))
                    .map(|(_, id)| *id)
                    .collect(),
                bank.iter()
                    .filter(|(idx, _)| marked.contains(*idx))
                    .map(|(_, id)| *id)
                    .collect(),
            )
        };

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
        for id in &survivors {
            roots.push(WorldRoot::Var(*id));
        }
        self.with_call_outs(|co| {
            for (_, call_out) in co.queue() {
                roots.push(WorldRoot::Ref(call_out.func_ref.clone()));
            }
        });

        let report = gc_pass(&self.committer_tx, dropped, roots).await?;

        // Cull the bank only after the committer has applied the drops.
        self.with_upvalues_mut(|cells| {
            cells.retain(|_, id| survivors.contains(id));
        });
        Ok(report)
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

    /// Read the upvalue bank. Production never reads the cells directly
    /// (value reads route through the committer), so this is test-only.
    #[cfg(test)]
    pub(crate) fn with_upvalues<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&GcVarIdBank) -> R,
    {
        f(&self.upvalues.read())
    }

    pub(crate) fn with_upvalues_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut GcVarIdBank) -> R,
    {
        f(&mut self.upvalues.write())
    }

    pub(crate) fn clone_upvalues(&self) -> Arc<RwLock<GcVarIdBank>> {
        self.upvalues.clone()
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

impl Mark for GlobalState {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        // The committer world is not a GC root here: payload vars committed into
        // it are unmarked and the sweep only culls upvalue cells, so a payload
        // reachable only through a committed slot is retained. Reclaiming the
        // dead ones is the quiescent pass's job (`GlobalState::gc`).
        //
        // No live tasks are marked: the pass runs only at quiescence, when no
        // transaction is in flight.
        self.object_space.mark(marked, processed)?;

        self.with_call_outs(|co| co.mark(marked, processed))
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
        test_support::test_config,
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

    /// A `FunctionPtr` whose dynamic receiver is the string at `path`.
    fn string_receiver_ptr(path: &str) -> FunctionPtr {
        FunctionPtrBuilder::default()
            .address(FunctionAddress::Dynamic(ustr("foo")))
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
    ) -> std::result::Result<(), Box<lpc_rs_errors::LpcError>> {
        let key = gs
            .object_space
            .path_key(LpcPath::InGame(std::path::PathBuf::from("/dynamic_receiver")).as_ref());
        let cell_id = gs.object_space.cell_id(&key);
        let live = start_txn(&gs.committer_tx).await?;
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        txn.with(|t| t.write_process(cell_id, process));
        let changeset = txn.with(|t| t.take_changeset());
        let commit = commit_changeset(&gs.committer_tx, changeset).await?;
        let effects = txn.with(|t| t.take_effects());
        drop(live);
        commit.expect("object cell commit must succeed");
        if !effects.is_empty() {
            flush_effects(&gs.config, &gs.object_space, gs.call_outs(), effects).await;
        }
        Ok(())
    }

    /// The committed cell is the single source of truth: a receiver whose
    /// cell is already committed resolves to that exact `Arc`.
    #[tokio::test]
    async fn string_receiver_finds_committed() {
        let gs = state_for_receiver();
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
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver"))
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
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver"))
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
        let live = start_txn(&gs.committer_tx).await.unwrap();
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        txn.with(|t| t.drop_var(cell_id));
        let changeset = txn.with(|t| t.take_changeset());
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
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver"))
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

        let got = gs
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver"))
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

        let ptr = string_receiver_ptr("/dynamic_receiver");
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

        let got = gs
            .resolve_dynamic_string_receiver(&string_receiver_ptr("/dynamic_receiver"))
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

        let target = compile_process_from_code(
            &gs.object_space,
            "/target.c",
            "int i = 123; int get() { return i; }",
        )
        .await
        .unwrap();
        ObjectSpace::insert_process_physical(&gs.object_space, target.clone());
        assert!(!gs.is_initialized(&target));

        let caller = compile_process_from_code(
            &gs.object_space,
            "/caller.c",
            r#"int r = "/target"->get();"#,
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

        let a = start_txn(&gs.committer_tx).await.unwrap();
        let b = start_txn(&gs.committer_tx).await.unwrap();
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
