use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_RELOAD,
    apply::valid_apply,
    efun::efun_context::EfunContext,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::Effect,
    vm::system_reload::{ReloadRequest, ReloadTarget},
};

pub async fn request_system_reload<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if context.txn().with(|t| t.reload_preparing) {
        return Err(context
            .runtime_error("request_system_reload: cannot request a reload during preparation"));
    }
    let name = context
        .arg(0)
        .as_str()
        .ok_or_else(|| context.runtime_error("request_system_reload: target must be a string"))?;
    let target = ReloadTarget::parse(name, context.task_context())?;
    let request = Arc::new(ReloadRequest {
        id: context.task_context().global_state.reloads.mint_id()?,
        target,
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
        VALID_RELOAD,
        &request.args(),
    )
    .await?
    {
        return Err(context.runtime_error("request_system_reload: permission denied"));
    }
    context.return_efun_result(request.id.into());
    context.record_effect(Effect::SystemReload(request));
    Ok(())
}

pub fn query_system_reload<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(id) = context.arg(0) else {
        return Err(context.runtime_error("query_system_reload: ID must be an int"));
    };
    let Some(status) = context.task_context().global_state.reloads.get(id.0) else {
        return Ok(());
    };
    if !status
        .request
        .caller
        .upgrade()
        .is_some_and(|owner| Arc::ptr_eq(&owner, context.process()))
    {
        return Err(context.runtime_error("query_system_reload: permission denied"));
    }
    let values: [(LpcRef, LpcRef); 3] = [
        ("target".into(), status.request.target.name().into()),
        ("state".into(), status.state.into()),
        ("error".into(), status.error.into()),
    ];
    context.return_mapping(LpcMapping::new(values.into_iter().collect()));
    Ok(())
}
