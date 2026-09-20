use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_RECOMPILE,
    apply::valid_apply,
    efun::efun_context::EfunContext,
    lpc_mapping::LpcMapping,
    lpc_ref::{LpcRef, NULL},
    stm::Effect,
    vm::object_recompile::{RecompileRequest, check_target},
};

pub async fn request_object_recompile<const N: usize>(
    context: &mut EfunContext<'_, N>,
) -> Result<()> {
    if context.txn().with(|t| t.reload_preparing) {
        return Err(context.runtime_error(
            "request_object_recompile: cannot request recompilation during preparation",
        ));
    }
    let target = context.arg(0).live_object(context.txn()).ok_or_else(|| {
        context.runtime_error("request_object_recompile: expected a live prototype")
    })?;
    check_target(context.task_context(), &target)?;
    let request = Arc::new(RecompileRequest {
        id: context
            .task_context()
            .global_state
            .recompilations
            .mint_id()?,
        target: Arc::downgrade(&target),
        caller: Arc::downgrade(context.process()),
        player: context
            .task_context()
            .this_player
            .load_full()
            .as_ref()
            .map(Arc::downgrade),
        program: context.calling_program().as_str().map(str::to_owned),
    });
    if !valid_apply(
        context.task_context(),
        Some(context.chain()),
        VALID_RECOMPILE,
        &request.args(),
    )
    .await?
    {
        return Err(context.runtime_error("request_object_recompile: permission denied"));
    }
    context.return_efun_result(request.id.into());
    context.record_effect(Effect::ObjectRecompile(request));
    Ok(())
}

pub fn query_object_recompile<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(id) = context.arg(0) else {
        return Err(context.runtime_error("query_object_recompile: ID must be an int"));
    };
    let Some(status) = context.task_context().global_state.recompilations.get(id.0) else {
        return Ok(());
    };
    if !status
        .request
        .caller
        .upgrade()
        .is_some_and(|caller| Arc::ptr_eq(&caller, context.process()))
    {
        return Err(context.runtime_error("query_object_recompile: permission denied"));
    }
    let target = status
        .request
        .target
        .upgrade()
        .filter(|target| target.is_live(context.txn()))
        .map_or(NULL, |target| Arc::downgrade(&target).into());
    let fields: [(LpcRef, LpcRef); 4] = [
        ("target".into(), target),
        ("state".into(), status.state.into()),
        ("error".into(), status.error.into()),
        ("updated".into(), (status.updated as i64).into()),
    ];
    context.return_mapping(LpcMapping::new(fields.into_iter().collect()));
    Ok(())
}
