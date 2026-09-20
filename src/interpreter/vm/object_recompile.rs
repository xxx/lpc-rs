//! In-place publication of a prototype's new program and its clones' state.

use std::{
    collections::{BTreeMap, HashMap},
    sync::{
        Arc, Weak,
        atomic::{AtomicI64, Ordering},
    },
};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::{LpcError, Result};
use parking_lot::Mutex;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        CLEAN_UP, VALID_RECOMPILE,
        apply::{report_warnings, valid_apply},
        compile_gate::MasterGate,
        file_view::TransactionSourceReader,
        lpc_ref::{LpcRef, NULL},
        process::{Process, ProgramImage},
        program::Program,
        stm::{
            AttemptBody, CommitProtocol, Conflict, Effect, LiveSnapshot, Transaction, TxnHandle,
            commit_changeset, flush_effects, start_txn,
        },
        task::{Task, task_template::TaskTemplate},
        task_context::{Caller, TaskContext},
        vm::global_state::GlobalState,
    },
    util::process_builder::compile_process_in_context,
};

/// A recompilation request whose caller's transaction has committed.
#[derive(Debug)]
pub struct RecompileRequest {
    pub(crate) id: i64,
    pub(crate) target: Weak<Process>,
    pub(crate) caller: Weak<Process>,
    pub(crate) player: Option<Weak<Process>>,
    pub(crate) program: Option<String>,
}

impl RecompileRequest {
    pub(crate) fn args(&self) -> [LpcRef; 3] {
        [
            self.target.clone().into(),
            self.caller.clone().into(),
            self.program.as_deref().map(LpcRef::from).unwrap_or(NULL),
        ]
    }
}

#[derive(Debug, Clone)]
pub(crate) struct RecompileStatus {
    pub request: Arc<RecompileRequest>,
    pub state: &'static str,
    pub error: String,
    pub updated: usize,
}

#[derive(Debug, Default)]
pub(crate) struct Recompilations {
    next: AtomicI64,
    entries: Mutex<BTreeMap<i64, RecompileStatus>>,
}

impl Recompilations {
    pub(crate) fn mint_id(&self) -> Result<i64> {
        self.next
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |n| n.checked_add(1))
            .map(|n| n + 1)
            .map_err(|_| LpcError::runtime("object recompilation request IDs exhausted"))
    }

    pub(crate) fn enqueue(&self, request: Arc<RecompileRequest>) {
        self.entries.lock().insert(
            request.id,
            RecompileStatus {
                request,
                state: "queued",
                error: String::new(),
                updated: 0,
            },
        );
    }

    pub(crate) fn get(&self, id: i64) -> Option<RecompileStatus> {
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

    pub(crate) fn finish(&self, id: i64, result: Result<usize>) {
        let mut entries = self.entries.lock();
        if let Some(status) = entries.get_mut(&id) {
            match result {
                Ok(updated) => {
                    status.state = "succeeded";
                    status.updated = updated;
                }
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

pub(crate) fn check_target(ctx: &TaskContext, target: &Arc<Process>) -> Result<()> {
    if !target.is_prototype() || !target.is_live(ctx.txn()) || !target.is_initialized(ctx.txn()) {
        return Err(LpcError::runtime(
            "object recompilation: expected a live, initialized prototype",
        ));
    }
    if ctx
        .object_space()
        .is_system_key(&ctx.object_space().process_key(target))
    {
        return Err(LpcError::runtime(
            "object recompilation: use request_system_reload for driver-owned objects",
        ));
    }
    Ok(())
}

fn check_member(ctx: &TaskContext, member: &Arc<Process>, program: &Program) -> Result<()> {
    if !member.is_live(ctx.txn()) || !member.is_initialized(ctx.txn()) {
        return Err(LpcError::runtime(
            "object recompilation: group member is retired or uninitialized",
        ));
    }
    let shadowed = ctx.txn().with(|t| {
        Process::shadow_target_in(t, member).is_some() || !Process::shadows_in(t, member).is_empty()
    });
    if shadowed {
        return Err(LpcError::runtime(
            "object recompilation: group member participates in a shadow chain",
        ));
    }
    let cleanup = !program.pragmas.resident() && program.unmangled_functions.contains_key(CLEAN_UP);
    if cleanup != member.cleanup.is_some() {
        return Err(LpcError::runtime(
            "object recompilation: changing cleanup eligibility is unsupported",
        ));
    }
    Ok(())
}

fn retained_globals(old: &Program, new: &Program) -> Vec<(RegisterSize, RegisterSize)> {
    let declarations: HashMap<_, _> = old
        .global_variable_info
        .iter()
        .map(|variable| ((&variable.filename, variable.name.as_str()), variable))
        .collect();
    new.global_variable_info
        .iter()
        .filter_map(|variable| {
            let old = declarations.get(&(&variable.filename, variable.name.as_str()))?;
            (old.type_ == variable.type_).then_some((old.slot, variable.slot))
        })
        .collect()
}

impl GlobalState {
    /// Run a committed request once, retrying the whole preparation on conflict.
    pub async fn run_object_recompile(self: &Arc<Self>, request: Arc<RecompileRequest>) {
        if !self.recompilations.start(request.id) {
            return;
        }
        let mut body = RecompileBody {
            gs: self,
            request: &request,
            txn: None,
            updated: 0,
        };
        let (result, _) = self.attempt_runner.run(&mut body).await;
        self.recompilations
            .finish(request.id, result.map(|()| body.updated));
    }
}

struct RecompileBody<'a> {
    gs: &'a Arc<GlobalState>,
    request: &'a RecompileRequest,
    txn: Option<TxnHandle>,
    updated: usize,
}

impl RecompileBody<'_> {
    async fn prepare(&mut self, ctx: &TaskContext) -> Result<()> {
        if !ctx.process.is_live(ctx.txn()) {
            return Err(LpcError::runtime(
                "object recompilation: requester is retired",
            ));
        }
        if self.request.player.is_some()
            && !ctx
                .this_player
                .load_full()
                .is_some_and(|p| p.is_live(ctx.txn()))
        {
            return Err(LpcError::runtime(
                "object recompilation: original command giver is gone",
            ));
        }
        let target = self
            .request
            .target
            .upgrade()
            .ok_or_else(|| LpcError::runtime("object recompilation: target is gone"))?;
        check_target(ctx, &target)?;
        let callers = Some(Caller::link(ctx.process.clone(), None));
        if !valid_apply(ctx, callers.clone(), VALID_RECOMPILE, &self.request.args()).await? {
            return Err(LpcError::runtime("object recompilation: permission denied"));
        }
        check_target(ctx, &target)?;
        let old = target.image(ctx.txn());
        let gate = MasterGate::new(ctx, callers.clone());
        let reader = Arc::new(TransactionSourceReader(ctx.txn().clone()));
        let compiling = ctx.txn().time_compilation();
        let (compiled, warnings) =
            compile_process_in_context(ctx, &old.program.filename, None, gate, reader).await?;
        drop(compiling);
        report_warnings(ctx, callers.clone(), &old.program.filename, warnings).await?;
        let mut program = compiled.initial_program().as_ref().clone();
        program.clones = old.program.clones.clone();
        let program = Arc::new(program);
        let retained = retained_globals(&old.program, &program);
        let clones = ctx.txn().with(|t| t.read_array(program.clones.id));
        let mut members = vec![target];
        if let Some(clones) = clones {
            for reference in clones.iter() {
                if let Some(clone) = reference.live_object(ctx.txn()) {
                    members.push(clone);
                }
            }
        }
        let mut migrations = Vec::with_capacity(members.len());
        for member in members {
            check_member(ctx, &member, &program)?;
            let old_image = member.image(ctx.txn());
            if !Arc::ptr_eq(&old_image.program, &old.program) {
                return Err(LpcError::bug("clone group contains a different program"));
            }
            let image = Arc::new(ProgramImage::migrate(
                program.clone(),
                &old_image,
                &retained,
            ));
            let values = ctx.txn().with(|t| {
                retained
                    .iter()
                    .map(|&(old_slot, new_slot)| {
                        (
                            image.var_id(new_slot),
                            t.read(old_image.var_id(old_slot)).unwrap_or(NULL),
                        )
                    })
                    .collect::<Vec<_>>()
            });
            migrations.push((member, image, values));
        }
        ctx.txn().with(|t| {
            for (member, image, _) in &migrations {
                t.write_image(member, image.clone());
            }
        });
        for (member, _, _) in &migrations {
            for region in &program.layout {
                let function = program.function(region.init).cloned().ok_or_else(|| {
                    LpcError::bug("global initializer missing from program layout")
                })?;
                let nested = ctx.nested(callers.clone(), member.clone())?;
                let mut task = Task::<MAX_CALL_STACK_SIZE>::new(nested);
                task.timed_eval(function, &[], ctx.config().max_execution_time)
                    .await?;
            }
        }
        for (member, _, values) in &migrations {
            check_member(ctx, member, &program)?;
            ctx.txn().with(|t| {
                for (cell, value) in values {
                    t.write(*cell, value.clone());
                }
            });
        }
        self.updated = migrations.len();
        Ok(())
    }
}

#[async_trait::async_trait]
impl AttemptBody for RecompileBody<'_> {
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
        self.updated = 0;
        let caller = self
            .request
            .caller
            .upgrade()
            .ok_or_else(|| LpcError::runtime("object recompilation: requester is gone"))?;
        txn.with(|t| {
            t.set_origin(&caller, "object_recompile");
            t.reload_preparing = true;
        });
        let mut template = TaskTemplate::from(self.gs.clone());
        template.txn = txn;
        template.set_this_player(self.request.player.as_ref().and_then(Weak::upgrade));
        self.prepare(&template.into_task_context(caller)).await?;
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
