//! In-place publication of new code and compatible state for live object groups.

use super::{
    object_update::UpdateRequest,
    system_recompile::{SystemTarget, SystemView, compatible_exports},
};
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
        stm::AuthorityView,
        task::Task,
        task_context::{Caller, Callers, TaskContext},
    },
    util::process_builder::compile_process_in_context,
};
use lpc_rs_core::RegisterSize;
use lpc_rs_errors::{LpcError, Result};
use std::{
    collections::HashMap,
    sync::{Arc, Weak},
};

#[derive(Debug, Clone)]
pub(crate) enum RecompileTarget {
    Object(Weak<Process>),
    System(SystemTarget),
}

impl RecompileTarget {
    pub(crate) fn parse(value: &LpcRef, ctx: &TaskContext) -> Result<Self> {
        if let Some(name) = value.as_str() {
            return Ok(Self::System(SystemTarget::parse(name, ctx)?));
        }
        let target = value.live_object(ctx.txn()).ok_or_else(|| {
            LpcError::runtime(
                "request_object_recompile: expected a live prototype or system selector",
            )
        })?;
        check_target(ctx, &target)?;
        Ok(Self::Object(Arc::downgrade(&target)))
    }

    pub(crate) fn value(&self, ctx: &TaskContext) -> LpcRef {
        match self {
            Self::Object(target) => target
                .upgrade()
                .filter(|p| p.is_live(ctx.txn()))
                .map_or(NULL, |p| Arc::downgrade(&p).into()),
            Self::System(target) => target.name().into(),
        }
    }

    /// Status remains inspectable after a target disappears or becomes ineligible for recompilation.
    pub(crate) fn status_prototypes(&self, ctx: &TaskContext) -> Vec<LpcRef> {
        match self {
            Self::Object(_) => vec![self.value(ctx)],
            Self::System(target) => {
                let mut prototypes = Vec::new();
                if *target != SystemTarget::Master {
                    prototypes.push(
                        ctx.simul_efuns()
                            .map_or(NULL, |p| Arc::downgrade(&p).into()),
                    );
                }
                if *target != SystemTarget::Simul {
                    prototypes.push(
                        ctx.master_object()
                            .map_or(NULL, |p| Arc::downgrade(&p).into()),
                    );
                }
                prototypes
            }
        }
    }

    fn resolve(&self, ctx: &TaskContext) -> Result<Vec<Arc<Process>>> {
        let targets = match self {
            Self::Object(target) => vec![
                target
                    .upgrade()
                    .ok_or_else(|| LpcError::runtime("object recompilation: target is gone"))?,
            ],
            Self::System(target) => {
                let mut targets = Vec::new();
                if *target != SystemTarget::Master {
                    targets.push(ctx.simul_efuns().ok_or_else(|| {
                        LpcError::runtime("object recompilation: no simul-efun object is loaded")
                    })?);
                }
                if *target != SystemTarget::Simul {
                    targets.push(ctx.master_object().ok_or_else(|| {
                        LpcError::runtime("object recompilation: no master is loaded")
                    })?);
                }
                targets
            }
        };
        for target in &targets {
            check_target(ctx, target)?;
        }
        Ok(targets)
    }
}

pub(crate) fn check_target(ctx: &TaskContext, target: &Arc<Process>) -> Result<()> {
    if !target.is_prototype() || !target.is_live(ctx.txn()) || !target.is_initialized(ctx.txn()) {
        return Err(LpcError::runtime(
            "object recompilation: expected a live, initialized prototype",
        ));
    }
    Ok(())
}

pub(crate) async fn authorize(
    ctx: &TaskContext,
    request: &UpdateRequest,
    target: &RecompileTarget,
    callers: Callers,
) -> Result<Vec<Arc<Process>>> {
    let targets = target.resolve(ctx)?;
    let master = ctx.master_object();
    let simul = ctx.simul_efuns();
    if master
        .as_ref()
        .zip(simul.as_ref())
        .is_some_and(|(m, s)| Arc::ptr_eq(m, s))
    {
        return Err(LpcError::runtime(
            "object recompilation: master and simul-efun paths must differ",
        ));
    }
    for target in &targets {
        if !valid_apply(
            ctx,
            callers.clone(),
            VALID_RECOMPILE,
            &request.args(Arc::downgrade(target).into()),
        )
        .await?
        {
            return Err(LpcError::runtime("object recompilation: permission denied"));
        }
        check_target(ctx, target)?;
    }
    Ok(targets)
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

pub(crate) async fn prepare(
    ctx: &mut TaskContext,
    request: &UpdateRequest,
    target: &RecompileTarget,
) -> Result<usize> {
    let callers = Some(Caller::link(ctx.process.clone(), None));
    let targets = authorize(ctx, request, target, callers.clone()).await?;
    let master = ctx.master_object();
    let simul = ctx.simul_efuns();
    let selected = |p: &Arc<Process>| targets.iter().any(|target| Arc::ptr_eq(target, p));
    let mut authority = AuthorityView::default();
    if let Some(master) = master.filter(|m| selected(m) || simul.as_ref().is_some_and(selected)) {
        authority.pin(ctx.txn(), &master, selected(&master));
        if let Some(simul) = &simul {
            authority.pin(ctx.txn(), simul, selected(simul));
        }
        ctx.system_view = Some(Arc::new(SystemView {
            master,
            simul: simul.clone(),
            authority: Arc::new(authority),
        }));
    }
    let mut updated = Vec::new();
    for target in targets {
        let is_simul = simul.as_ref().is_some_and(|s| Arc::ptr_eq(s, &target));
        updated.extend(recompile_group(ctx, target, is_simul, callers.clone()).await?);
    }
    for member in &updated {
        check_member(ctx, member, &member.program(ctx.txn()))?;
    }
    Ok(updated.len())
}

async fn recompile_group(
    ctx: &TaskContext,
    target: Arc<Process>,
    is_simul: bool,
    callers: Callers,
) -> Result<Vec<Arc<Process>>> {
    let old = target.image(ctx.txn());
    let gate = MasterGate::new(ctx, callers.clone());
    let reader = Arc::new(TransactionSourceReader(ctx.txn().clone()));
    let compiling = ctx.txn().time_compilation();
    let (compiled, warnings) =
        compile_process_in_context(ctx, &old.program.filename, None, gate, reader).await?;
    drop(compiling);
    report_warnings(ctx, callers.clone(), &old.program.filename, warnings).await?;
    if is_simul {
        compatible_exports(&old.program, compiled.initial_program())?;
    }
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
            let function = program
                .function(region.init)
                .cloned()
                .ok_or_else(|| LpcError::bug("global initializer missing from program layout"))?;
            let nested = ctx.nested(callers.clone(), member.clone())?;
            let mut task = Task::<MAX_CALL_STACK_SIZE>::new(nested);
            task.timed_eval(function, &[], ctx.config().max_execution_time)
                .await?;
        }
    }
    for (member, _, values) in &migrations {
        check_member(ctx, member, &program)?;
        for (cell, value) in values {
            let retained = ctx
                .system_view
                .as_ref()
                .and_then(|view| view.authority.retained(ctx.txn(), *cell))
                .unwrap_or_else(|| value.clone());
            ctx.txn().with(|t| t.write(*cell, retained));
        }
    }
    Ok(migrations
        .into_iter()
        .map(|(member, _, _)| member)
        .collect())
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod system_tests;

#[cfg(test)]
mod callback_tests;
