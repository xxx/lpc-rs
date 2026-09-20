use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_RELOAD,
    apply::valid_apply,
    efun::efun_context::EfunContext,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::Effect,
    vm::{
        object_recompile::{RecompileTarget, authorize},
        object_update::{UpdateRequest, UpdateTarget},
        system_reload::ReloadTarget,
    },
};

pub async fn request_object_recompile<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    request(ctx, true).await
}

pub async fn request_system_reload<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    request(ctx, false).await
}

async fn request<const N: usize>(ctx: &mut EfunContext<'_, N>, recompile: bool) -> Result<()> {
    if ctx.txn().with(|t| t.reload_preparing) {
        return Err(ctx.runtime_error("object update: cannot request an update during preparation"));
    }
    let target = if recompile {
        UpdateTarget::Recompile(RecompileTarget::parse(ctx.arg(0), ctx.task_context())?)
    } else {
        let name = ctx
            .arg(0)
            .as_str()
            .ok_or_else(|| ctx.runtime_error("request_system_reload: target must be a string"))?;
        UpdateTarget::Restart(ReloadTarget::parse(name, ctx.task_context())?)
    };
    let request = Arc::new(UpdateRequest {
        id: ctx.task_context().global_state.updates.mint_id()?,
        target,
        caller: Arc::downgrade(ctx.process()),
        player: ctx
            .task_context()
            .this_player
            .load_full()
            .as_ref()
            .map(Arc::downgrade),
        program: ctx.calling_program().as_str().map(str::to_owned),
    });
    let callers = Some(ctx.chain());
    match &request.target {
        UpdateTarget::Recompile(target) => {
            authorize(ctx.task_context(), &request, target, callers).await?;
        }
        UpdateTarget::Restart(target) => {
            if !valid_apply(
                ctx.task_context(),
                callers,
                VALID_RELOAD,
                &request.args(target.name().into()),
            )
            .await?
            {
                return Err(ctx.runtime_error("request_system_reload: permission denied"));
            }
        }
    }
    ctx.return_efun_result(request.id.into());
    ctx.record_effect(Effect::ObjectUpdate(request));
    Ok(())
}

pub fn query_object_recompile<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    query(ctx, true)
}

pub fn query_system_reload<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    query(ctx, false)
}

fn query<const N: usize>(ctx: &mut EfunContext<'_, N>, recompile: bool) -> Result<()> {
    let LpcRef::Int(id) = ctx.arg(0) else {
        return Err(ctx.runtime_error("object update: ID must be an int"));
    };
    let Some(status) = ctx.task_context().global_state.updates.get(id.0) else {
        return Ok(());
    };
    if matches!(status.request.target, UpdateTarget::Recompile(_)) != recompile {
        return Ok(());
    }
    if !status
        .request
        .caller
        .upgrade()
        .is_some_and(|caller| Arc::ptr_eq(&caller, ctx.process()))
    {
        return Err(ctx.runtime_error("object update status: permission denied"));
    }
    let target = match &status.request.target {
        UpdateTarget::Recompile(target) => target.value(ctx.task_context()),
        UpdateTarget::Restart(target) => target.name().into(),
    };
    let mut fields = vec![
        (LpcRef::from("target"), target),
        (LpcRef::from("state"), status.state.into()),
        (LpcRef::from("error"), status.error.into()),
    ];
    if recompile {
        fields.push(("updated".into(), (status.updated as i64).into()));
    }
    ctx.return_mapping(LpcMapping::new(fields.into_iter().collect()));
    Ok(())
}
