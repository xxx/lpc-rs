use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_SNOOP, apply::valid_apply, efun::efun_context::EfunContext, lpc_ref::LpcRef,
    process::Process, snooping,
};

fn object<const N: usize>(
    context: &EfunContext<'_, N>,
    index: usize,
) -> Result<Option<Arc<Process>>> {
    let value = context.arg(index);
    if value.is_null_in(context.txn()) {
        return Ok(None);
    }
    if !matches!(value, LpcRef::Object(_)) {
        return Err(context.runtime_error("snoop: expected an object"));
    }
    Ok(value.live_object(context.txn()))
}

async fn authorize<const N: usize>(
    context: &EfunContext<'_, N>,
    observer: Option<&Arc<Process>>,
    target: Option<&Arc<Process>>,
    operation: &str,
) -> Result<()> {
    let value = |process: Option<&Arc<Process>>| {
        process
            .map(|p| LpcRef::from(Arc::downgrade(p)))
            .unwrap_or_else(|| context.txn().nil())
    };
    let args = [
        value(Some(context.process())),
        value(observer),
        value(target),
        operation.into(),
        context.calling_program(),
    ];
    if !valid_apply(
        context.task_context(),
        Some(context.chain()),
        VALID_SNOOP,
        &args,
    )
    .await?
    {
        return Err(context.runtime_error("snoop: permission denied"));
    }
    Ok(())
}

pub async fn snoop<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let observer =
        object(context, 0)?.ok_or_else(|| context.runtime_error("snoop: observer is not live"))?;
    let target = object(context, 1)?;
    if target.is_none() && matches!(context.arg(1), LpcRef::Object(_)) {
        return Err(context.runtime_error("snoop: target is not live"));
    }
    if let Some(target) = target {
        authorize(context, Some(&observer), Some(&target), "start").await?;
        snooping::start(
            context.txn(),
            &observer,
            &target,
            context.config().max_pending_snoop,
        )?;
        context.return_efun_result(LpcRef::from(Arc::downgrade(&target)));
    } else {
        let old = context.txn().with(|t| snooping::outgoing(t, &observer));
        let target = old.as_ref().and_then(|r| r.target.upgrade());
        authorize(context, Some(&observer), target.as_ref(), "stop").await?;
        if !observer.is_live(context.txn()) {
            return Err(context.runtime_error("snoop: observer was destroyed"));
        }
        let current = context.txn().with(|t| snooping::outgoing(t, &observer));
        if current.as_ref().map(|r| r.id) != old.as_ref().map(|r| r.id) {
            return Err(context.runtime_error("snoop: relationship changed during authorization"));
        }
        if let Some(current) = current {
            context.txn().with(|t| snooping::retire(t, &current));
        }
        context.return_efun_result(LpcRef::from(Arc::downgrade(&observer)));
    }
    Ok(())
}

async fn query<const N: usize>(context: &mut EfunContext<'_, N>, outgoing: bool) -> Result<()> {
    let Some(process) = object(context, 0)? else {
        context.return_efun_result(context.txn().nil());
        return Ok(());
    };
    if outgoing {
        authorize(context, Some(&process), None, "query_snooping").await?;
    } else {
        authorize(context, None, Some(&process), "query_snoop").await?;
    }
    let result = context.txn().with(|t| {
        if !process.is_live_in(t) {
            return None;
        }
        let registration = if outgoing {
            snooping::outgoing(t, &process)
        } else {
            snooping::incoming(t, &process)
        }?;
        if !registration.valid(t) {
            return None;
        }
        if outgoing {
            registration.target.upgrade()
        } else {
            registration.observer.upgrade()
        }
    });
    context.return_efun_result(
        result
            .map(|p| LpcRef::from(Arc::downgrade(&p)))
            .unwrap_or_else(|| context.txn().nil()),
    );
    Ok(())
}

pub async fn query_snoop<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    query(context, false).await
}
pub async fn query_snooping<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    query(context, true).await
}
