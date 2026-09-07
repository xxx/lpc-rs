use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    QUERY_ALLOW_SHADOW,
    apply::master_apply,
    efun::efun_context::EfunContext,
    lpc_int::LpcInt,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    program::Program,
    stm::TxnHandle,
    task_context::TaskContext,
};

/// `shadow(ob, flag)`: with a true `flag` (the default) the calling object
/// becomes the outermost shadow of `ob` and `ob` is returned; with `flag`
/// 0, the object shadowing `ob`, or 0. Every refusal is a runtime error
/// naming its reason.
pub async fn shadow<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Object(weak) = context.arg(0) else {
        return Err(context.runtime_error("shadow: the target must be an object"));
    };
    let Some(target) = weak.upgrade() else {
        return Err(context.runtime_error("shadow: the target has been destructed"));
    };
    let query = context.arg_count() > 1 && matches!(context.arg(1), LpcRef::Int(LpcInt(0)));
    let txn = context.txn().clone();
    if query {
        let answer = Process::shadow_after(&txn, &target)
            .map_or(NULL, |ob| LpcRef::from(Arc::downgrade(&ob)));
        context.return_efun_result(answer);
        return Ok(());
    }

    let caller = context.process().clone();
    let refuse = |reason: String| format!("shadow '{caller}' on '{target}': {reason}");
    if let Some(reason) = structural_refusal(&txn, context.task_context(), &caller, &target) {
        return Err(context.runtime_error(refuse(reason)));
    }
    let master = context.task_context().object_space().master_object();
    let defined = master.as_ref().is_some_and(|m| {
        m.program
            .unmangled_functions
            .contains_key(QUERY_ALLOW_SHADOW)
    });
    if !defined {
        return Err(context.runtime_error(refuse(
            "Shadowing is disabled: the master defines no query_allow_shadow.".to_string(),
        )));
    }
    let args = [LpcRef::from(Arc::downgrade(&target))];
    let allowed = master_apply(
        context.task_context(),
        Some(context.chain()),
        QUERY_ALLOW_SHADOW,
        &args,
    )
    .await?
    .is_some_and(|verdict| verdict.is_truthy(&txn));
    if !allowed {
        return Err(context.runtime_error(refuse("The master refused the shadow.".to_string())));
    }

    Process::attach_shadow(&txn, &caller, &target);
    context.return_efun_result(LpcRef::from(Arc::downgrade(&target)));
    Ok(())
}

/// The first reason `caller` may not shadow `target` that needs no master,
/// in CD's order; `None` when there is none.
fn structural_refusal(
    txn: &TxnHandle,
    ctx: &TaskContext,
    caller: &Arc<Process>,
    target: &Arc<Process>,
) -> Option<String> {
    if Process::shadow_target(txn, caller).is_some() {
        return Some("Already shadowing.".into());
    }
    if !Process::shadows_of(txn, caller).is_empty() {
        return Some("Can't shadow when shadowed.".into());
    }
    if Process::environment_of(txn, caller).is_some() {
        return Some("The shadow must not reside inside another object.".into());
    }
    if Arc::ptr_eq(caller, target) || Process::shadow_target(txn, target).is_some() {
        return Some("Can't shadow a shadow.".into());
    }
    if ctx
        .object_space()
        .master_object()
        .is_some_and(|master| Arc::ptr_eq(&master, target))
    {
        return Some("Can't shadow the master.".into());
    }
    if ctx
        .simul_efuns()
        .is_some_and(|sefuns| Arc::ptr_eq(sefuns, target))
    {
        return Some("Can't shadow the simul-efun object.".into());
    }
    if target.program.pragmas.no_shadow() {
        return Some("Can't shadow a 'no_shadow' program.".into());
    }
    nomask_clash(&caller.program, &target.program)
        .map(|name| format!("Illegal to shadow 'nomask' function '{name}'."))
}

/// The name of a `nomask` function of `target` (own or inherited) that
/// `shadow` also defines, if any.
fn nomask_clash(shadow: &Program, target: &Program) -> Option<String> {
    target
        .functions
        .values()
        .filter(|function| function.prototype.flags.nomask())
        .find(|function| shadow.contains_function(function.name().as_ref()))
        .map(|function| function.name().to_string())
}
