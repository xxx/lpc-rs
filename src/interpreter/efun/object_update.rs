use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_RECOMPILE,
    apply::valid_apply,
    efun::efun_context::EfunContext,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::Effect,
    vm::{
        object_recompile::{RecompileTarget, authorize},
        object_update::UpdateRequest,
    },
};

pub async fn request_object_recompile<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    if ctx.txn().with(|t| t.recompile_preparing) {
        return Err(ctx.runtime_error("object update: cannot request an update during preparation"));
    }
    let target = RecompileTarget::parse(ctx.arg(0), ctx.task_context())?;
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
    authorize(ctx.task_context(), &request, &request.target, callers).await?;
    ctx.return_efun_result(request.id.into());
    ctx.record_effect(Effect::ObjectUpdate(request));
    Ok(())
}

pub async fn query_object_recompile<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(id) = ctx.arg(0) else {
        return Err(ctx.runtime_error("object update: ID must be an int"));
    };
    let Some(status) = ctx.task_context().global_state.updates.get(id.0) else {
        return Ok(());
    };
    if !status
        .request
        .caller
        .upgrade()
        .is_some_and(|caller| Arc::ptr_eq(&caller, ctx.process()))
    {
        let caller = LpcRef::from(Arc::downgrade(ctx.process()));
        let program = ctx.calling_program();
        let callers = Some(ctx.chain());
        for prototype in status.request.target.status_prototypes(ctx.task_context()) {
            if !valid_apply(
                ctx.task_context(),
                callers.clone(),
                VALID_RECOMPILE,
                &[prototype, caller.clone(), program.clone()],
            )
            .await?
            {
                return Err(ctx.runtime_error("object update status: permission denied"));
            }
        }
    }
    let target = status.request.target.value(ctx.task_context());
    let fields = vec![
        (LpcRef::from("target"), target),
        (LpcRef::from("state"), status.state.into()),
        (LpcRef::from("error"), status.error.into()),
        ("updated".into(), (status.updated as i64).into()),
    ];
    ctx.return_mapping(LpcMapping::new(fields.into_iter().collect()));
    Ok(())
}
