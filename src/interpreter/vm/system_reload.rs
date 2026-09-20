//! Transactional preparation and publication of the driver-owned objects.

use std::{collections::BTreeMap, sync::Arc};

use super::object_update::UpdateRequest;
use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        VALID_RELOAD,
        apply::{report_warnings, valid_apply},
        compile_gate::MasterGate,
        file_view::TransactionSourceReader,
        process::Process,
        program::Program,
        task::Task,
        task_context::{Caller, ObjectLookup, TaskContext},
    },
    util::process_builder::compile_process_in_context,
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};

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
                    "system update: expected master, simul_efun, or both",
                ));
            }
        };
        if target != Self::Master && ctx.config().simul_efun_source().is_none() {
            return Err(LpcError::runtime(
                "system update: no simul-efun source configured",
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

#[derive(Debug, Clone)]
pub(crate) struct SystemView {
    pub master: Arc<Process>,
    pub authority: Option<Arc<crate::interpreter::stm::AuthorityView>>,
    pub simul: Option<Arc<Process>>,
    pub authority_simul: Option<Arc<Process>>,
    pub staged: BTreeMap<String, Arc<Process>>,
}

pub(crate) async fn prepare(
    ctx: &mut TaskContext,
    request: &UpdateRequest,
    target: ReloadTarget,
) -> Result<usize> {
    let callers = Some(Caller::link(ctx.process.clone(), None));
    if !valid_apply(
        ctx,
        callers.clone(),
        VALID_RELOAD,
        &request.args(target.name().into()),
    )
    .await?
    {
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
    if target != ReloadTarget::Master {
        let old = simul
            .clone()
            .ok_or_else(|| LpcError::runtime("system reload: no simul-efun object is loaded"))?;
        targets.push((
            ctx.config().simul_efun_source().ok_or_else(|| {
                LpcError::runtime("system reload: no simul-efun source configured")
            })?,
            old,
            true,
        ));
    }
    if target != ReloadTarget::Simul {
        let path = LpcPath::new_in_game(
            ctx.config().master_object.as_str(),
            "/",
            &*ctx.config().lib_dir,
        );
        targets.push((path, master.clone(), false));
    }
    let mut view = SystemView {
        master,
        authority: None,
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
        let (new, warnings) = compile_process_in_context(ctx, &path, None, gate, reader).await?;
        drop(compiling);
        report_warnings(
            ctx,
            callers.clone(),
            &new.initial_program().filename,
            warnings,
        )
        .await?;
        if is_simul {
            compatible_exports(&old.program(ctx.txn()), new.initial_program())?;
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
    let updated = replacements.len();
    for (_, old, new) in replacements {
        ctx.cancel_process_call_outs(&old);
        ctx.insert_process_transactional(&new);
    }
    Ok(updated)
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

pub(crate) fn compatible_exports(old: &Program, new: &Program) -> Result<()> {
    for (name, function) in old.unmangled_functions.iter() {
        if name == lpc_rs_core::INIT_PROGRAM || name == lpc_rs_core::INIT_GLOBALS {
            continue;
        }
        let Some(replacement) = new.unmangled_functions.get(name) else {
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

#[cfg(test)]
mod tests;
