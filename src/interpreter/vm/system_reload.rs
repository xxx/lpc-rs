//! Transactional preparation and publication of the driver-owned objects.

use std::{
    collections::BTreeMap,
    sync::{
        Arc, Weak,
        atomic::{AtomicI64, Ordering},
    },
};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};
use parking_lot::Mutex;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        VALID_RELOAD,
        apply::{report_warnings, valid_apply},
        compile_gate::MasterGate,
        file_view::TransactionSourceReader,
        lpc_ref::LpcRef,
        process::Process,
        stm::{
            AttemptBody, CommitProtocol, Conflict, Effect, LiveSnapshot, Transaction, TxnHandle,
            commit_changeset, flush_effects, start_txn,
        },
        task::{Task, task_template::TaskTemplate},
        task_context::{Caller, ObjectLookup, TaskContext},
        vm::global_state::GlobalState,
    },
    util::process_builder::compile_process_in_context,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ReloadTarget {
    Master,
    Simul,
    Both,
}

impl ReloadTarget {
    pub(crate) fn parse(value: &str, ctx: &TaskContext) -> Result<Self> {
        let target = match value {
            "master" => Self::Master,
            "simul_efun" => Self::Simul,
            "both" => Self::Both,
            _ => {
                return Err(LpcError::runtime(
                    "request_system_reload: expected master, simul_efun, or both",
                ));
            }
        };
        if target != Self::Master && ctx.config().simul_efun_source().is_none() {
            return Err(LpcError::runtime(
                "request_system_reload: no simul-efun source configured",
            ));
        }
        Ok(target)
    }

    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::Master => "master",
            Self::Simul => "simul_efun",
            Self::Both => "both",
        }
    }
}

/// A committed administrative request delivered to the VM.
#[derive(Debug)]
pub struct ReloadRequest {
    pub(crate) id: i64,
    pub(crate) target: ReloadTarget,
    pub(crate) caller: Weak<Process>,
    pub(crate) player: Option<Weak<Process>>,
    pub(crate) program: Option<String>,
}

impl ReloadRequest {
    pub(crate) fn args(&self) -> [LpcRef; 3] {
        [
            self.target.name().into(),
            self.caller.clone().into(),
            self.program
                .as_deref()
                .map(LpcRef::from)
                .unwrap_or_else(|| 0.into()),
        ]
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ReloadStatus {
    pub request: Arc<ReloadRequest>,
    pub state: &'static str,
    pub error: String,
}

#[derive(Debug, Default)]
pub(crate) struct Reloads {
    next: AtomicI64,
    entries: Mutex<BTreeMap<i64, ReloadStatus>>,
}

impl Reloads {
    pub(crate) fn mint_id(&self) -> Result<i64> {
        self.next
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |n| n.checked_add(1))
            .map(|n| n + 1)
            .map_err(|_| LpcError::runtime("system reload request IDs exhausted"))
    }

    pub(crate) fn enqueue(&self, request: Arc<ReloadRequest>) {
        self.entries.lock().insert(
            request.id,
            ReloadStatus {
                request,
                state: "queued",
                error: String::new(),
            },
        );
    }

    pub(crate) fn get(&self, id: i64) -> Option<ReloadStatus> {
        self.entries.lock().get(&id).cloned()
    }

    fn start(&self, id: i64) -> bool {
        let mut entries = self.entries.lock();
        let Some(status) = entries.get_mut(&id) else {
            return false;
        };
        if status.state != "queued" {
            return false;
        }
        status.state = "running";
        true
    }

    pub(crate) fn finish(&self, id: i64, result: Result<()>) {
        let mut entries = self.entries.lock();
        if let Some(status) = entries.get_mut(&id) {
            match result {
                Ok(()) => status.state = "succeeded",
                Err(error) => {
                    status.state = "failed";
                    status.error = error.diagnostic_string();
                }
            }
        }
        let finished: Vec<_> = entries
            .iter()
            .filter(|(_, s)| matches!(s.state, "succeeded" | "failed"))
            .map(|(&id, _)| id)
            .collect();
        for id in finished.iter().take(finished.len().saturating_sub(128)) {
            entries.remove(id);
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SystemView {
    pub master: Arc<Process>,
    pub simul: Option<Arc<Process>>,
    pub authority_simul: Option<Arc<Process>>,
    pub staged: BTreeMap<String, Arc<Process>>,
}

impl GlobalState {
    /// Execute one committed reload request; repeated delivery is ignored.
    pub async fn run_system_reload(self: &Arc<Self>, request: Arc<ReloadRequest>) {
        if !self.reloads.start(request.id) {
            return;
        }
        let mut body = ReloadBody {
            gs: self,
            request: &request,
            txn: None,
        };
        let (result, _) = self.attempt_runner.run(&mut body).await;
        self.reloads.finish(request.id, result);
    }
}

struct ReloadBody<'a> {
    gs: &'a Arc<GlobalState>,
    request: &'a ReloadRequest,
    txn: Option<TxnHandle>,
}

impl ReloadBody<'_> {
    async fn prepare(&self, ctx: &mut TaskContext) -> Result<()> {
        let caller = ctx.process.clone();
        if self.request.player.is_some()
            && !ctx
                .this_player
                .load_full()
                .is_some_and(|p| p.is_live(ctx.txn()))
        {
            return Err(LpcError::runtime(
                "system reload: original command giver is gone",
            ));
        }
        if !caller.is_live(ctx.txn()) {
            return Err(LpcError::runtime("system reload: requester is retired"));
        }
        let callers = Some(Caller::link(caller, None));
        if !valid_apply(ctx, callers.clone(), VALID_RELOAD, &self.request.args()).await? {
            return Err(LpcError::runtime("system reload: permission denied"));
        }
        let master = ctx
            .master_object()
            .ok_or_else(|| LpcError::runtime("system reload: no master is loaded"))?;
        let simul = ctx.simul_efuns();
        if simul
            .as_ref()
            .is_some_and(|simul| Arc::ptr_eq(simul, &master))
        {
            return Err(LpcError::runtime(
                "system reload: master and simul-efun paths must differ",
            ));
        }
        let mut targets = Vec::new();
        if self.request.target != ReloadTarget::Master {
            let old = simul.clone().ok_or_else(|| {
                LpcError::runtime("system reload: no simul-efun object is loaded")
            })?;
            targets.push((
                self.gs.config.simul_efun_source().ok_or_else(|| {
                    LpcError::runtime("system reload: no simul-efun source configured")
                })?,
                old,
                true,
            ));
        }
        if self.request.target != ReloadTarget::Simul {
            let path = LpcPath::new_in_game(
                self.gs.config.master_object.as_str(),
                "/",
                &*self.gs.config.lib_dir,
            );
            targets.push((path, master.clone(), false));
        }
        let mut view = SystemView {
            master,
            authority_simul: simul.clone(),
            simul,
            staged: BTreeMap::new(),
        };
        ctx.system_view = Some(Arc::new(view.clone()));
        let mut replacements = Vec::new();
        for (path, old, is_simul) in targets {
            ensure_replaceable(ctx, &old)?;
            let gate = MasterGate::new(ctx, callers.clone());
            let reader = Arc::new(TransactionSourceReader(ctx.txn().clone()));
            let compiling = ctx.txn().time_compilation();
            let (new, warnings) =
                compile_process_in_context(ctx, &path, None, gate, reader).await?;
            drop(compiling);
            report_warnings(
                ctx,
                callers.clone(),
                &new.initial_program().filename,
                warnings,
            )
            .await?;
            if is_simul {
                compatible_exports(&old, &new)?;
                view.simul = Some(new.clone());
            }
            view.staged
                .insert(ctx.object_space().process_key(&new), new.clone());
            ctx.system_view = Some(Arc::new(view.clone()));
            let nested = ctx.nested(callers.clone(), new.clone())?;
            Task::<MAX_CALL_STACK_SIZE>::initialize_process(nested).await?;
            ensure_replaceable(ctx, &new)?;
            replacements.push((path, old, new));
        }
        for (path, old, _) in &replacements {
            match crate::interpreter::stm::txn_find_object(ctx.txn(), ctx.object_space(), path) {
                ObjectLookup::Found(current) if Arc::ptr_eq(&current, old) => {}
                _ => {
                    return Err(LpcError::runtime(
                        "system reload: initializer changed an active system object",
                    ));
                }
            }
            ensure_replaceable(ctx, old)?;
        }
        for (_, old, new) in replacements {
            ctx.cancel_process_call_outs(&old);
            ctx.insert_process_transactional(&new);
        }
        Ok(())
    }
}

fn ensure_replaceable(ctx: &TaskContext, process: &Arc<Process>) -> Result<()> {
    if !process.is_live(ctx.txn())
        || process.commands_enabled(ctx.txn())
        || Process::environment_of(ctx.txn(), process).is_some()
        || !Process::inventory_of(ctx.txn(), process).is_empty()
        || ctx
            .txn()
            .with(|t| t.read_connection(process.connection.id))
            .is_some()
        || process.ever_in_a_chain()
        || process.parser_ready.get().is_some()
    {
        return Err(LpcError::runtime(
            "system reload: target must be a live, detached daemon without commands, parser registration, or shadows",
        ));
    }
    Ok(())
}

fn compatible_exports(old: &Process, new: &Process) -> Result<()> {
    for (name, function) in old.initial_program().unmangled_functions.iter() {
        if name == lpc_rs_core::INIT_PROGRAM || name == lpc_rs_core::INIT_GLOBALS {
            continue;
        }
        let Some(replacement) = new.initial_program().unmangled_functions.get(name) else {
            return Err(LpcError::runtime(format!(
                "system reload: simul-efun export `{name}` was removed"
            )));
        };
        let a = &function.prototype;
        let b = &replacement.prototype;
        if a.return_type != b.return_type
            || a.arity != b.arity
            || a.arg_types != b.arg_types
            || a.ref_params != b.ref_params
            || a.ref_tail != b.ref_tail
            || a.flags != b.flags
        {
            return Err(LpcError::runtime(format!(
                "system reload: incompatible simul-efun export `{name}`"
            )));
        }
    }
    Ok(())
}

#[async_trait::async_trait]
impl AttemptBody for ReloadBody<'_> {
    fn timeout_ms(&self) -> u64 {
        self.gs.config.max_execution_time
    }
    fn take_compilation_time(&mut self) -> std::time::Duration {
        self.txn.as_ref().map_or(std::time::Duration::ZERO, |txn| {
            txn.with(|t| t.take_compilation_time())
        })
    }
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = start_txn(tx).await?;
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        self.txn = Some(txn.clone());
        let caller = self
            .request
            .caller
            .upgrade()
            .ok_or_else(|| LpcError::runtime("system reload: requester is gone"))?;
        txn.with(|t| {
            t.set_origin(&caller, "system_reload");
            t.reload_preparing = true;
        });
        let mut template = TaskTemplate::from(self.gs.clone());
        template.txn = txn;
        template.set_this_player(self.request.player.as_ref().and_then(Weak::upgrade));
        let mut ctx = template.into_task_context(caller);
        self.prepare(&mut ctx).await?;
        Ok(Some(live))
    }
    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
        let txn = self.txn.as_ref().expect("attempt opened");
        let result = commit_changeset(tx, txn.with(|t| t.take_changeset())).await?;
        Ok((result, txn.with(|t| t.take_effects())))
    }
    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        flush_effects(self.gs, effects).await;
        Ok(())
    }
}

#[cfg(test)]
mod tests;
