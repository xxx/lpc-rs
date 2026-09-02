use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use arc_swap::ArcSwapAny;
use chrono::Duration;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result, lpc_bug};
use lpc_rs_utils::config::Config;
use thin_vec::ThinVec;
use tokio::sync::mpsc::Sender;

use crate::compile_time_config::{MAX_CALL_STACK_SIZE, MAX_TASK_CHAIN};
use crate::{
    interpreter::{
        COMPILE_OBJECT, VALID_LOAD,
        apply::{master_apply, report_warnings, valid_apply},
        compile_gate::MasterGate,
        lpc_ref::LpcRef,
        object_space::ObjectSpace,
        process::Process,
        stm::{
            CallOutSchedule, TxnHandle, VarId, txn_find_object, txn_insert_process, txn_undo_insert,
        },
        task::Task,
        vm::{global_state::GlobalState, vm_op::VmOp},
    },
    util::{get_simul_efuns, process_builder::compile_process_from_path},
};

/// The command a task is running: what `query_verb`, `query_command`, and
/// `notify_fail` read and write.
#[derive(Debug, Default)]
pub struct CommandState {
    /// The line `dispatch` is trying, after any `process_input` rewrite.
    pub line: String,
    /// The first word as typed.
    pub verb_typed: String,
    /// What `query_verb()` returns for the rule being tried.
    pub verb_reported: String,
    /// The `notify_fail` argument (a string or a function pointer), set by
    /// the efun of the same name.
    pub notify_fail: Option<LpcRef>,
}

/// The task's final result, resettable between retry attempts.
#[derive(Debug)]
pub struct TaskResult {
    inner: std::sync::Mutex<Option<LpcRef>>,
}

impl TaskResult {
    pub fn new() -> Self {
        Self {
            inner: std::sync::Mutex::new(None),
        }
    }

    /// Set the result.
    pub fn set(&self, new_result: LpcRef) -> Result<()> {
        let mut g = self.inner.lock().unwrap();
        if g.is_some() {
            return Err(lpc_bug!("TaskResult::set result already set"));
        }
        *g = Some(new_result);
        Ok(())
    }

    /// Return a clone of the stored result. `LpcRef` is cheap to clone
    /// (small ints, or `Arc`/`Weak` payloads) and avoids borrow-checker handling
    /// on the hot path.
    pub fn get(&self) -> Option<LpcRef> {
        self.inner.lock().unwrap().clone()
    }

    pub fn into_inner(self) -> Option<LpcRef> {
        self.inner.into_inner().unwrap()
    }

    /// Clear the result (for a retry re-run).
    pub fn reset(&self) {
        *self.inner.lock().unwrap() = None;
    }
}

impl Default for TaskResult {
    fn default() -> Self {
        Self::new()
    }
}

/// The outcome of a transactional object lookup.
///
/// `create-on-miss` (re-compiling and inserting) is only legal on
/// `NotCreated`: on `Removed` it would resurrect an object this attempt
/// destructed.
#[derive(Debug)]
pub enum ObjectLookup {
    /// Live for this attempt — its cell holds it, or the physical map does
    /// (objects created outside any transaction: bootstrap, `clone_object`).
    Found(Arc<Process>),
    /// This attempt destructed it.
    Removed,
    /// No cell exists.
    NotCreated,
}

/// Who asks the master for a load: the efun or opcode, the object executing
/// it, and the defining file of its code (`0` when there is none).
#[derive(Debug, Clone)]
pub struct Loader {
    /// The efun's name, or `"call_other"` for `->`.
    pub func: String,
    /// The object executing the load.
    pub caller: Arc<Process>,
    /// The defining file of the calling code, or `0`.
    pub program: LpcRef,
}

/// `arg` as an object path for `func`: resolved against `cwd` and confined
/// to the lib. An escape is `` <func>: `<arg>` is not a valid path ``, span-less;
/// the door adds its span.
pub(crate) fn confine_object_path(
    config: &Config,
    arg: &str,
    cwd: impl AsRef<Path>,
    func: &str,
) -> Result<LpcPath> {
    let path = LpcPath::new_in_game(arg, cwd, &*config.lib_dir);
    config
        .validate_in_game_path(&path, None)
        .map_err(|_| LpcError::runtime(format!("{func}: `{arg}` is not a valid path")))?;
    Ok(path)
}

/// A struct to carry context during the evaluation of a single [`Task`].
#[derive(Debug)]
pub struct TaskContext {
    /// The [`GlobalState`] from the [`Vm`](crate::interpreter::vm::Vm).
    pub global_state: Arc<GlobalState>,

    /// The [`Process`] that owns the function being
    /// called in this [`Task`].
    // TODO: this is not accurate in the case of some call_others, or function ptr calls,
    //       The process in the call frame is more accurate, and this probably should be removed.
    pub process: Arc<Process>,

    /// Direct pointer to the simul efuns
    pub simul_efuns: Option<Arc<Process>>,

    /// The final result of the original function that was called.
    pub result: TaskResult,

    /// The command giver, if there was one. This might be an NPC, or None.
    // TODO: put this into an Arc so it can shared more easily between multiple contexts.
    //       also make this Weak
    pub this_player: ArcSwapAny<Option<Arc<Process>>>,

    /// The upvalue_ptrs to populate the initial frame with, if any.
    pub upvalue_ptrs: Option<ThinVec<VarId>>,

    /// The number of this task in the current chain of Tasks. This is
    /// used to prevent infinite recursion among multiple Tasks.
    pub chain_count: u8,

    /// This task's transaction handle. Top-level tasks re-base it per
    /// attempt; sub-tasks adopt their caller's handle.
    pub(crate) txn: TxnHandle,

    /// Commands in progress, innermost last; shared by every clone of this
    /// context so a handler's `query_verb()` sees the dispatch that called it.
    pub(crate) command: Arc<parking_lot::Mutex<Vec<CommandState>>>,
}

impl TaskContext {
    /// Create a new [`TaskContext`]
    pub fn new<P>(
        global_state: Arc<GlobalState>,
        process: P,
        this_player: Option<Arc<Process>>,
    ) -> Self
    where
        P: Into<Arc<Process>>,
    {
        let simul_efuns = get_simul_efuns(&global_state.config, &global_state.object_space);

        Self {
            global_state,
            process: process.into(),
            result: TaskResult::new(),
            simul_efuns,
            this_player: ArcSwapAny::from(this_player),
            upvalue_ptrs: None,
            chain_count: 0,
            txn: TxnHandle::default(),
            command: Arc::default(),
        }
    }

    /// Set the process for an existing TaskContext
    pub fn with_process<P>(mut self, process: P) -> Self
    where
        P: Into<Arc<Process>>,
    {
        self.process = process.into();

        self
    }

    /// The context for a task nested under this one in `process`, one level
    /// deeper in the chain; an error past `MAX_TASK_CHAIN`.
    pub fn nested<P>(&self, process: P) -> Result<Self>
    where
        P: Into<Arc<Process>>,
    {
        if self.chain_count >= MAX_TASK_CHAIN {
            return Err(LpcError::runtime(format!(
                "nested task depth of {MAX_TASK_CHAIN} exceeded"
            )));
        }
        let mut nested = self.clone().with_process(process);
        nested.chain_count = self.chain_count + 1;
        Ok(nested)
    }

    /// Find an object by path, transactionally: read the object's cell first
    /// (a cell read makes a concurrent create of this object conflict this
    /// transaction and re-run it), then fall back to the physical map for
    /// objects created outside this transaction (bootstrap, `clone_object`).
    /// Does not initialize or create (use `load_object` / the site's own
    /// create step for that).
    pub fn find_object(&self, path: &LpcPath) -> ObjectLookup {
        txn_find_object(self.txn(), self.object_space(), path)
    }

    /// `confine_object_path` under this context's config.
    pub fn object_path(&self, arg: &str, cwd: impl AsRef<Path>, func: &str) -> Result<LpcPath> {
        confine_object_path(self.config(), arg, cwd, func)
    }

    /// The one LPC-triggered compile. `path` is confined already. When its
    /// source, `<key>.c`, exists it is compiled after `valid_load` hears it;
    /// a path with no source is put to the master's `compile_object` instead
    /// and becomes the answered blueprint's program under `path`. Either
    /// way the result is not inserted; a clone-suffixed path is never
    /// virtual.
    pub async fn compile_process(&self, path: &LpcPath, loader: &Loader) -> Result<Arc<Process>> {
        if path.is_clone() || self.source_exists(path).await {
            return self.compile_file_process(path, loader).await;
        }
        self.instantiate_virtual(path, loader).await
    }

    /// Whether `path`'s source is a regular file in the lib.
    async fn source_exists(&self, path: &LpcPath) -> bool {
        let source = path.source_file();
        tokio::fs::metadata(source.as_server(self.config().lib_dir.as_str()))
            .await
            .is_ok_and(|m| m.is_file())
    }

    /// Compile `path`'s source, `<key>.c`, after `valid_load` hears it, with
    /// the master's compile-time gate installed. Not inserted; the compile's
    /// warnings go to `warning_handler`. A missing source is the compiler's
    /// `Cannot read file` error.
    async fn compile_file_process(&self, path: &LpcPath, loader: &Loader) -> Result<Arc<Process>> {
        let source = path
            .source_file()
            .as_in_game(self.config().lib_dir.as_str())
            .display()
            .to_string();
        let args = [
            LpcRef::from(source),
            LpcRef::from(loader.func.as_str()),
            LpcRef::from(Arc::downgrade(&loader.caller)),
            loader.program.clone(),
        ];
        if !valid_apply(self, VALID_LOAD, &args).await? {
            return Err(LpcError::runtime(format!(
                "{}: permission denied",
                loader.func
            )));
        }
        let gate = MasterGate::new(self);
        let (process, warnings) =
            compile_process_from_path(self.object_space(), path, Some(gate)).await?;
        report_warnings(self, &process.program.filename, warnings).await?;
        Ok(process)
    }

    /// `path` resident, else its file compiled for `loader`, inserted and
    /// initialized in a nested task. Never `compile_object`: one level of
    /// indirection only.
    async fn load_file_process(&self, path: &LpcPath, loader: &Loader) -> Result<Arc<Process>> {
        match self.find_object(path) {
            ObjectLookup::Found(process) => Ok(process),
            ObjectLookup::Removed => Err(LpcError::runtime(format!(
                "{}: `{}` was destructed in this task",
                COMPILE_OBJECT,
                AsRef::<str>::as_ref(path)
            ))),
            ObjectLookup::NotCreated => {
                let process = self.compile_file_process(path, loader).await?;
                self.insert_and_initialize(&process).await?;
                Ok(process)
            }
        }
    }

    /// A path with no source: the master's `compile_object` names a
    /// blueprint, and the result is that blueprint's program under `path`.
    /// The answer is a name, not a permission — the blueprint is loaded for
    /// `loader`, so `valid_load` still hears the requester. A declined ask
    /// fails as the missing file it is.
    async fn instantiate_virtual(&self, path: &LpcPath, loader: &Loader) -> Result<Arc<Process>> {
        let key = self.object_space().path_key(AsRef::<str>::as_ref(path));
        let args = [
            LpcRef::from(key.as_str()),
            LpcRef::from(loader.func.as_str()),
            LpcRef::from(Arc::downgrade(&loader.caller)),
            loader.program.clone(),
        ];
        let Some(answer) = master_apply(self, COMPILE_OBJECT, &args).await? else {
            return self.compile_file_process(path, loader).await;
        };
        let blueprint = match answer.as_str() {
            Some(name) => name.to_owned(),
            None if !answer.is_truthy(self.txn()) => {
                return self.compile_file_process(path, loader).await;
            }
            None => {
                return Err(LpcError::runtime(format!(
                    "{COMPILE_OBJECT}: expected a path, got {answer}"
                )));
            }
        };
        let blueprint = confine_object_path(self.config(), &blueprint, "/", COMPILE_OBJECT)?;
        let blueprint = self.load_file_process(&blueprint, loader).await?;
        if blueprint.program.pragmas.no_clone() {
            return Err(LpcError::runtime(format!(
                "{COMPILE_OBJECT}: {} has `#pragma no_clone` enabled, and so cannot be instantiated",
                blueprint.program.filename
            )));
        }
        Ok(Arc::new(Process::new_virtual(
            blueprint.program.clone(),
            key,
        )))
    }

    /// Insert an object transactionally: write its cell and record a deferred
    /// physical insert. The cell write is what makes the object usable within
    /// this transaction (and what concurrent readers conflict against); the
    /// effect is what places it in the physical map at commit.
    pub fn insert_process_transactional(&self, process: &Arc<Process>) {
        txn_insert_process(self.txn(), self.object_space(), process)
    }

    /// The one insert-and-initialize door every load site shares: insert
    /// `process` transactionally, then run its initializer in a nested task
    /// riding this attempt. Any failure — a refused nested task or a
    /// throwing initializer — leaves nothing under the object's key, and the
    /// error returns unchanged.
    pub async fn insert_and_initialize(&self, process: &Arc<Process>) -> Result<()> {
        // `nested` is resolved before the insert: a depth refusal here must
        // not leave an inserted, never-initialized object behind.
        let nested = self.nested(process.clone())?;
        self.insert_process_transactional(process);
        if let Err(e) = Box::pin(Task::<MAX_CALL_STACK_SIZE>::initialize_process(nested)).await {
            txn_undo_insert(self.txn(), self.object_space(), process);
            return Err(e);
        }
        Ok(())
    }

    /// The one call-out interface: schedule a call out transactionally. The
    /// ID is minted from the per-VM counter (a physical read); the scheduling
    /// is a deferred effect.
    pub fn schedule_call_out(
        &self,
        owner: &Arc<Process>,
        func_ref: LpcRef,
        delay: Duration,
        repeat: Option<Duration>,
    ) -> u64 {
        let id = self.global_state.with_call_outs(|co| co.mint_id());
        self.txn().with(|t| {
            t.record_call_out(CallOutSchedule {
                id,
                process: Arc::downgrade(owner),
                func_ref,
                delay,
                repeat,
            })
        });
        id
    }

    /// Query a call out transactionally: `(pending - shadow) ∪ (physical - shadow)`.
    /// Returns an owned 4-field array
    /// (`[object, function, ms remaining, ms repeat]`), or `None` when the ID is
    /// unknown or this attempt canceled it. The caller mints the result cell
    /// after the scan.
    pub fn query_call_out(&self, id: u64) -> Option<Vec<LpcRef>> {
        if self.txn().with(|t| t.is_cancelled_call_out(id)) {
            return None;
        }
        let pending = self
            .txn()
            .with(|t| t.pending_call_outs().iter().find(|s| s.id == id).cloned());
        let pending_view = pending.map(|s| {
            vec![
                s.process.clone().into(),
                s.func_ref,
                LpcRef::Int(s.delay.num_milliseconds().into()),
                LpcRef::Int(s.repeat.map(|d| d.num_milliseconds()).unwrap_or(0).into()),
            ]
        });
        if pending_view.is_some() {
            return pending_view;
        }
        self.global_state.with_call_outs(|co| {
            co.get_by_id(id).map(|call_out| {
                vec![
                    call_out.process().clone().into(),
                    call_out.func_ref.clone(),
                    LpcRef::Int(
                        call_out
                            .time_remaining()
                            .map(|d| d.num_milliseconds())
                            .unwrap_or(0)
                            .into(),
                    ),
                    LpcRef::Int(
                        call_out
                            .repeat_duration()
                            .map(|d| d.num_milliseconds())
                            .unwrap_or(0)
                            .into(),
                    ),
                ]
            })
        })
    }

    /// Query all call outs owned by `owner`, as in
    /// [`TaskContext::query_call_out`].
    pub fn query_call_outs(&self, owner: &Arc<Process>) -> Vec<Vec<LpcRef>> {
        let mut out = Vec::new();
        let pending = self.txn().with(|t| t.pending_call_outs().to_vec());
        for s in pending {
            if self.txn().with(|t| t.is_cancelled_call_out(s.id)) {
                continue;
            }
            if let Some(p) = s.process.upgrade()
                && Arc::ptr_eq(&p, owner)
            {
                out.push(vec![
                    Arc::downgrade(&p).into(),
                    s.func_ref,
                    LpcRef::Int(s.delay.num_milliseconds().into()),
                    LpcRef::Int(s.repeat.map(|d| d.num_milliseconds()).unwrap_or(0).into()),
                ]);
            }
        }
        self.global_state.with_call_outs(|co| {
            for (_idx, call_out) in co.queue().iter() {
                if self.txn().with(|t| t.is_cancelled_call_out(call_out.id)) {
                    continue;
                }
                if let Some(p) = call_out.process().upgrade()
                    && Arc::ptr_eq(&p, owner)
                {
                    out.push(vec![
                        Arc::downgrade(&p).into(),
                        call_out.func_ref.clone(),
                        LpcRef::Int(
                            call_out
                                .time_remaining()
                                .map(|d| d.num_milliseconds())
                                .unwrap_or(0)
                                .into(),
                        ),
                        LpcRef::Int(
                            call_out
                                .repeat_duration()
                                .map(|d| d.num_milliseconds())
                                .unwrap_or(0)
                                .into(),
                        ),
                    ]);
                }
            }
            out
        })
    }

    /// Cancel a call out transactionally. One tier: pending entries drop
    /// from the attempt; committed entries get a deferred removal plus the
    /// shadow. Returns the milliseconds left, or -1 if unknown.
    pub fn cancel_call_out(&self, id: u64) -> i64 {
        if let Some(ms) = self.txn().with(|t| t.cancel_pending_call_out(id)) {
            return ms;
        }
        let remaining = self.global_state.with_call_outs(|co| {
            co.get_by_id(id).map(|call_out| {
                call_out
                    .time_remaining()
                    .map(|d| d.num_milliseconds())
                    .unwrap_or(0)
            })
        });
        match remaining {
            Some(ms) => {
                self.txn().with(|t| t.cancel_committed_call_out(id));
                ms
            }
            None => -1,
        }
    }

    /// The in-game directory of `process`'s name (lib root stripped): what
    /// its relative object paths resolve against.
    pub fn in_game_cwd_of(&self, process: &Process) -> PathBuf {
        let current_cwd = process.cwd();

        match current_cwd.strip_prefix(&*self.config().lib_dir) {
            Ok(x) => {
                if x.as_os_str().is_empty() {
                    PathBuf::from("/")
                } else if x.starts_with("/") {
                    x.to_path_buf()
                } else {
                    PathBuf::from(format!("/{}", x.display()))
                }
            }
            Err(_e) => current_cwd,
        }
    }

    /// Update the context's `result` with the passed [`LpcRef`]
    #[inline]
    pub fn set_result(&self, new_result: LpcRef) -> Result<()> {
        self.result.set(new_result)
    }

    /// Consume this context, and return its `result` field.
    #[inline]
    pub fn into_result(self) -> Option<LpcRef> {
        self.result.into_inner()
    }

    /// Return the [`Config`] used for the task
    #[inline]
    pub fn config(&self) -> &Arc<Config> {
        &self.global_state.config
    }

    /// Return the current pointer to the simul_efuns, if any
    #[inline]
    pub fn simul_efuns(&self) -> Option<&Arc<Process>> {
        self.simul_efuns.as_ref()
    }

    /// Return the [`Process`] that the task roots from.
    /// This *does not* change over the life of the task.
    #[inline]
    pub fn process(&self) -> &Arc<Process> {
        &self.process
    }

    /// The transaction this task runs in.
    #[inline]
    pub(crate) fn txn(&self) -> &TxnHandle {
        &self.txn
    }

    /// Run `f` over the innermost command in progress, if any.
    pub(crate) fn with_command<R>(&self, f: impl FnOnce(Option<&mut CommandState>) -> R) -> R {
        let mut commands = self.command.lock();
        f(commands.last_mut())
    }

    /// Return the [`ObjectSpace`]
    #[inline]
    pub fn object_space(&self) -> &Arc<ObjectSpace> {
        &self.global_state.object_space
    }

    /// Get the final result of the Task that this context is for, if it's
    /// finished. Returns an owned clone (see [`TaskResult::get`]).
    #[inline]
    pub fn result(&self) -> Option<LpcRef> {
        self.result.get()
    }

    /// Clear the result so a retry re-run can record a fresh one.
    #[inline]
    pub fn reset(&self) {
        self.result.reset()
    }

    /// Drop every command in progress; a joiner shares its parent's stack and
    /// must never call this.
    #[inline]
    pub(crate) fn clear_commands(&self) {
        self.command.lock().clear();
    }

    /// Get the `tx` channel for this task
    #[inline]
    pub fn tx(&self) -> Sender<VmOp> {
        self.global_state.tx.clone()
    }
}

impl Clone for TaskContext {
    fn clone(&self) -> Self {
        Self {
            global_state: self.global_state.clone(),
            process: self.process.clone(),
            result: TaskResult::new(),
            simul_efuns: self.simul_efuns.clone(),
            this_player: ArcSwapAny::from(self.this_player.load_full()),
            upvalue_ptrs: self.upvalue_ptrs.clone(),
            chain_count: self.chain_count,
            // Cloning the handle shares the in-flight transaction.
            txn: self.txn.clone(),
            command: self.command.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use tokio::sync::mpsc;

    use super::*;
    use crate::{
        interpreter::{lpc_ref::LpcRef, program::ProgramBuilder},
        test_support::test_config,
    };

    fn result_test_ctx() -> TaskContext {
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_server("./tests/fixtures/code/foo/bar/baz.c"))
            .build()
            .unwrap();
        let process = Process::new(program);
        let (tx, _rx) = mpsc::channel(100);
        let global_state = GlobalState::new(test_config(), tx);
        TaskContext::new(Arc::new(global_state), process, None)
    }

    #[test]
    fn result_set_get_reset_cycle() {
        let c = result_test_ctx();
        assert!(c.result().is_none());
        c.set_result(LpcRef::from(7i64)).unwrap();
        assert_eq!(c.result(), Some(LpcRef::from(7i64)));
        c.reset();
        assert!(c.result().is_none());
        c.set_result(LpcRef::from(8i64)).unwrap();
        assert_eq!(c.result(), Some(LpcRef::from(8i64)));
    }

    #[test]
    #[should_panic(expected = "already set")]
    fn result_double_set_is_a_bug() {
        let c = result_test_ctx();
        c.set_result(LpcRef::from(1i64)).unwrap();
        c.set_result(LpcRef::from(2i64)).unwrap();
    }

    #[test]
    fn test_in_game_cwd_of() {
        let config = test_config();
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_server("./tests/fixtures/code/foo/bar/baz.c"))
            .build()
            .unwrap();
        let process = Process::new(program);
        let (tx, _rx) = mpsc::channel(100);
        let global_state = GlobalState::new(config, tx);
        let context = TaskContext::new(Arc::new(global_state), process, None);

        assert_eq!(
            context.in_game_cwd_of(&context.process).to_str().unwrap(),
            "/foo/bar"
        );
    }
}
