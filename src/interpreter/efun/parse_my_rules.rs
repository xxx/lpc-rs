//! Parse only `this_object()`'s rules, returning arguments or executing the action.

use lpc_rs_errors::Result;

use crate::{
    command::{parser::Completion, trial},
    interpreter::{efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef},
};

/// `parse_my_rules(actor, line, execute = 0)` uses the explicit actor for every callback.
pub async fn parse_my_rules<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(actor) = context.arg(0).live_object(context.txn()) else {
        return Err(context.runtime_error("parse_my_rules: the actor is not a living"));
    };
    if !actor.commands_enabled(context.txn()) {
        return Err(context.runtime_error("parse_my_rules: the actor is not a living"));
    }
    let LpcRef::String(line) = context.arg(1).clone() else {
        return Err(context.runtime_error("parse_my_rules: the line must be a string"));
    };
    let completion = match context.try_arg(2) {
        None | Some(LpcRef::Int(LpcInt(0))) => Completion::Arguments,
        Some(LpcRef::Int(_)) => Completion::Execute,
        _ => return Err(context.runtime_error("parse_my_rules: execute must be an int")),
    };
    let owner = context.process().clone();
    let outcome = Box::pin(trial::owned_sentence(
        context.task_context(),
        &actor,
        &owner,
        line.to_str(),
        completion,
    ))
    .await?;
    context.return_efun_result(outcome.into_value(context.txn()));
    Ok(())
}
