//! Authorized TCP admission, inspection and synchronous ownership offers.

use std::{
    net::{IpAddr, SocketAddr},
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use lpc_rs_errors::{LpcError, Result};

use crate::interpreter::{
    VALID_SOCKET,
    apply::valid_apply,
    continuation::{Callee, Continuation, Next},
    efun::{bytes::int_arg, callback::function_arg, efun_context::EfunContext},
    function_type::{
        function_address::FunctionAddress,
        function_ptr::{FunctionPtr, FunctionPtrBuilder},
    },
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    process::Process,
    sockets::{SocketAccess, SocketHandle, SocketInfo, SocketState, SocketTarget},
    stm::TxnHandle,
};

#[cfg(test)]
mod tests;

fn handle<const N: usize>(ctx: &EfunContext<'_, N>, name: &str) -> Result<SocketHandle> {
    SocketHandle::try_from(int_arg(ctx, name, 0)?)
}

fn callback<const N: usize>(
    ctx: &EfunContext<'_, N>,
    name: &str,
    index: usize,
) -> Result<Arc<FunctionPtr>> {
    let pointer = function_arg(ctx, name, index)?;
    if !pointer
        .owner
        .upgrade()
        .is_some_and(|p| p.is_live(ctx.txn()))
        || !pointer.receiver_is_live(ctx.txn())
    {
        return Err(ctx.runtime_error(format!("{name}: callback is not live")));
    }
    Ok(pointer)
}

fn caller_live<const N: usize>(ctx: &EfunContext<'_, N>) -> Result<()> {
    if !ctx.process().is_live(ctx.txn()) {
        return Err(ctx.runtime_error("socket: caller is not live"));
    }
    Ok(())
}

fn owned<const N: usize>(
    ctx: &EfunContext<'_, N>,
    handle: SocketHandle,
    established: bool,
) -> Result<SocketInfo> {
    caller_live(ctx)?;
    let info = SocketAccess::new(ctx.task_context())
        .registration(handle)
        .ok_or_else(|| ctx.runtime_error("socket: stale handle"))?;
    if !std::ptr::eq(info.owner.as_ptr(), Arc::as_ptr(ctx.process())) {
        return Err(ctx.runtime_error("socket: wrong owner"));
    }
    if established && info.state != SocketState::Open {
        return Err(ctx.runtime_error("socket: only established streams can be offered"));
    }
    Ok(info)
}

fn endpoint(info: &SocketInfo) -> (String, u16) {
    let known = match info.target {
        SocketTarget::Connect { .. } => info.remote,
        SocketTarget::Listen(_) => info.local,
    };
    known.map_or_else(|| (String::new(), 0), |a| (a.ip().to_string(), a.port()))
}

async fn authorize<const N: usize>(
    ctx: &EfunContext<'_, N>,
    operation: &str,
    handle: i64,
    owner: Option<&Arc<Process>>,
    host: &str,
    port: u16,
    recipient: Option<&Arc<Process>>,
) -> Result<()> {
    caller_live(ctx)?;
    let object =
        |p: Option<&Arc<Process>>| p.map_or_else(|| ctx.txn().nil(), |p| Arc::downgrade(p).into());
    let info = LpcRef::Array(ctx.txn().with(|t| {
        t.mint_array(LpcArray::new(vec![
            handle.into(),
            object(owner),
            host.into(),
            i64::from(port).into(),
            object(recipient),
        ]))
    }));
    if !valid_apply(
        ctx.task_context(),
        Some(ctx.chain()),
        VALID_SOCKET,
        &[object(Some(ctx.process())), operation.into(), info],
    )
    .await?
    {
        return Err(ctx.runtime_error(format!("socket: permission denied for {operation}")));
    }
    caller_live(ctx)
}

async fn authorize_owned<const N: usize>(
    ctx: &EfunContext<'_, N>,
    operation: &str,
    handle: SocketHandle,
    recipient: Option<&Arc<Process>>,
    established: bool,
) -> Result<()> {
    let info = owned(ctx, handle, established)?;
    let (host, port) = endpoint(&info);
    authorize(
        ctx,
        operation,
        handle.get(),
        Some(ctx.process()),
        &host,
        port,
        recipient,
    )
    .await?;
    owned(ctx, handle, established)?;
    Ok(())
}

async fn create<const N: usize>(ctx: &mut EfunContext<'_, N>, listen: bool) -> Result<()> {
    let name = if listen {
        "socket_listen"
    } else {
        "socket_connect"
    };
    let host = ctx
        .arg(0)
        .as_str()
        .ok_or_else(|| ctx.runtime_error(format!("{name}: expected a string")))?
        .to_owned();
    let port = u16::try_from(int_arg(ctx, name, 1)?)
        .map_err(|_| ctx.runtime_error(format!("{name}: port is out of range")))?;
    if host.is_empty() || host.contains('\0') || (!listen && port == 0) {
        return Err(ctx.runtime_error(format!("{name}: invalid endpoint")));
    }
    let address = if listen {
        Some(
            host.parse::<IpAddr>()
                .map_err(|_| ctx.runtime_error("socket_listen: expected a numeric address"))?,
        )
    } else {
        None
    };
    callback(ctx, name, 2)?;
    authorize(
        ctx,
        if listen { "listen" } else { "connect" },
        -1,
        Some(ctx.process()),
        &host,
        port,
        None,
    )
    .await?;
    let pointer = callback(ctx, name, 2)?;
    let access = SocketAccess::new(ctx.task_context());
    let handle = match address {
        Some(address) => access.listen(ctx.process(), SocketAddr::new(address, port), pointer)?,
        None => access.connect(ctx.process(), host, port, pointer)?,
    };
    ctx.return_efun_result(handle.get().into());
    Ok(())
}

pub async fn socket_connect<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    create(ctx, false).await
}
pub async fn socket_listen<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    create(ctx, true).await
}

pub async fn socket_write<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    let handle = handle(ctx, "socket_write")?;
    let LpcRef::Bytes(data) = ctx.arg(1) else {
        return Err(ctx.runtime_error("socket_write: expected bytes"));
    };
    let data = data.as_ref().clone();
    authorize_owned(ctx, "write", handle, None, false).await?;
    let admitted = SocketAccess::new(ctx.task_context()).write(ctx.process(), handle, data)?;
    ctx.return_efun_result(admitted.into());
    Ok(())
}

pub async fn socket_close<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    let handle = handle(ctx, "socket_close")?;
    authorize_owned(ctx, "close", handle, None, false).await?;
    SocketAccess::new(ctx.task_context()).close(ctx.process(), handle)?;
    ctx.return_efun_result(ctx.txn().nil());
    Ok(())
}

fn row(txn: &TxnHandle, info: SocketInfo) -> LpcRef {
    let address = |a: Option<SocketAddr>| {
        a.map_or_else(
            || txn.nil(),
            |a| {
                LpcRef::Array(txn.with(|t| {
                    t.mint_array(LpcArray::new(vec![
                        a.ip().to_string().into(),
                        i64::from(a.port()).into(),
                    ]))
                }))
            },
        )
    };
    let kind = match info.target {
        SocketTarget::Connect { .. } => "stream",
        SocketTarget::Listen(_) => "listener",
    };
    let state = match info.state {
        SocketState::Connecting => "connecting",
        SocketState::Binding => "binding",
        SocketState::Open => "open",
        SocketState::Listening => "listening",
        SocketState::Closing => "closing",
        SocketState::Closed => "closed",
    };
    let mut map = LpcMapping::default();
    for (key, value) in [
        ("handle", info.handle.get().into()),
        ("owner", info.owner.into()),
        ("kind", kind.into()),
        ("state", state.into()),
        ("local", address(info.local)),
        ("remote", address(info.remote)),
        ("output_pending", LpcRef::from(info.output_pending as i64)),
        ("read_eof", info.read_eof.into()),
    ] {
        map.insert(key.into(), value);
    }
    LpcRef::Mapping(txn.with(|t| t.mint_mapping(map)))
}

pub async fn socket_info<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    let handle = handle(ctx, "socket_info")?;
    let before = SocketAccess::new(ctx.task_context()).registration(handle);
    let owner = before.as_ref().and_then(|info| info.owner.upgrade());
    let (host, port) = before.as_ref().map_or_else(|| (String::new(), 0), endpoint);
    authorize(ctx, "info", handle.get(), owner.as_ref(), &host, port, None).await?;
    let after = SocketAccess::new(ctx.task_context()).info(handle);
    if let (Some(before), Some(after)) = (&before, &after)
        && !std::sync::Weak::ptr_eq(&before.owner, &after.owner)
    {
        return Err(ctx.runtime_error("socket_info: ownership changed during authorization"));
    }
    let result = after.map_or_else(|| ctx.txn().nil(), |info| row(ctx.txn(), info));
    ctx.return_efun_result(result);
    Ok(())
}

pub async fn query_sockets<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    let owner = match ctx.arg(0) {
        LpcRef::Int(value) if value.0 == -1 => None,
        LpcRef::Object(_) => Some(
            ctx.arg(0)
                .live_object(ctx.txn())
                .ok_or_else(|| ctx.runtime_error("query_sockets: owner is not live"))?,
        ),
        value if ctx.arg_count() == 0 || value == &ctx.txn().nil() => Some(ctx.process().clone()),
        _ => return Err(ctx.runtime_error("query_sockets: expected an object or -1")),
    };
    authorize(ctx, "query", -1, owner.as_ref(), "", 0, None).await?;
    let rows: Vec<_> = SocketAccess::new(ctx.task_context())
        .query(owner.as_ref())?
        .into_iter()
        .map(|info| row(ctx.txn(), info))
        .collect();
    ctx.return_array(rows);
    Ok(())
}

#[derive(Debug)]
pub(crate) struct Offer {
    handle: SocketHandle,
    previous: Arc<Process>,
    recipient: Arc<Process>,
    acquired: AtomicBool,
}

#[derive(Debug, Clone)]
struct Release {
    offer: Arc<Offer>,
    call: Option<Callee>,
}

impl Continuation for Release {
    fn advance(&mut self, _result: Option<LpcRef>, _txn: &TxnHandle) -> Result<Next> {
        Ok(match self.call.take() {
            Some(call) => Next::Call(call),
            None => Next::Done(self.offer.acquired.load(Ordering::Relaxed).into()),
        })
    }
    fn clone_box(&self) -> Box<dyn Continuation> {
        Box::new(self.clone())
    }
    fn socket_offer(&self) -> Option<&Arc<Offer>> {
        Some(&self.offer)
    }
}

pub async fn socket_release<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    if ctx.socket_offer().is_some() {
        return Err(ctx.runtime_error("socket_release: recursive offers are not allowed"));
    }
    let handle = handle(ctx, "socket_release")?;
    let recipient = ctx
        .arg(1)
        .live_object(ctx.txn())
        .ok_or_else(|| ctx.runtime_error("socket_release: recipient is not live"))?;
    let pointer = callback(ctx, "socket_release", 2)?;
    authorize_owned(ctx, "release", handle, Some(&recipient), true).await?;
    let resolved = pointer
        .prepare_call(
            &[handle.get().into(), Arc::downgrade(ctx.process()).into()],
            ctx.task_context(),
            || Ok(Some(ctx.chain())),
        )
        .await?
        .ok_or_else(|| ctx.runtime_error("socket_release: offer has no target"))?;
    if !Arc::ptr_eq(&resolved.process, &recipient) {
        return Err(ctx.runtime_error("socket_release: offer must resolve to the recipient"));
    }
    owned(ctx, handle, true)?;
    callback(ctx, "socket_release", 2)?;
    let address = if resolved.function.prototype.is_efun() {
        FunctionAddress::Efun(ustr::ustr(resolved.function.name()))
    } else {
        FunctionAddress::in_image(
            &recipient,
            resolved.function,
            &pointer.frame_receiver(recipient.clone(), ctx.txn()).image,
        )
    };
    // Freeze the checked receiver and bound arguments before entering the offer.
    let pointer = Arc::new(
        FunctionPtrBuilder::default()
            .owner(pointer.owner.clone())
            .address(address)
            .origin(pointer.origin.clone())
            .upvalue_ptrs(pointer.upvalue_ptrs.clone())
            .build()
            .map_err(|e| LpcError::runtime(e.to_string()))?,
    );
    let offer = Arc::new(Offer {
        handle,
        previous: ctx.process().clone(),
        recipient,
        acquired: AtomicBool::new(false),
    });
    ctx.continue_with(Box::new(Release {
        offer,
        call: Some(Callee::Pointer {
            ptr: pointer,
            args: resolved.args.into_iter().collect(),
        }),
    }));
    Ok(())
}

pub async fn socket_acquire<const N: usize>(ctx: &mut EfunContext<'_, N>) -> Result<()> {
    let handle = handle(ctx, "socket_acquire")?;
    let offer = ctx
        .socket_offer()
        .ok_or_else(|| ctx.runtime_error("socket_acquire: no active offer"))?;
    let check = || -> Result<()> {
        if handle != offer.handle
            || !Arc::ptr_eq(ctx.process(), &offer.recipient)
            || offer.acquired.load(Ordering::Relaxed)
            || !offer.previous.is_live(ctx.txn())
            || !offer.recipient.is_live(ctx.txn())
        {
            return Err(ctx.runtime_error("socket_acquire: invalid offer context"));
        }
        let info = SocketAccess::new(ctx.task_context())
            .registration(handle)
            .ok_or_else(|| ctx.runtime_error("socket_acquire: stale handle"))?;
        if !std::ptr::eq(info.owner.as_ptr(), Arc::as_ptr(&offer.previous))
            || info.state != SocketState::Open
        {
            return Err(ctx.runtime_error("socket_acquire: offered stream changed"));
        }
        Ok(())
    };
    check()?;
    callback(ctx, "socket_acquire", 1)?;
    let info = SocketAccess::new(ctx.task_context())
        .registration(handle)
        .ok_or_else(|| ctx.runtime_error("socket_acquire: stale handle"))?;
    let (host, port) = endpoint(&info);
    authorize(
        ctx,
        "acquire",
        handle.get(),
        Some(&offer.previous),
        &host,
        port,
        Some(&offer.recipient),
    )
    .await?;
    check()?;
    let pointer = callback(ctx, "socket_acquire", 1)?;
    SocketAccess::new(ctx.task_context()).transfer(
        handle,
        &offer.previous,
        &offer.recipient,
        pointer,
    )?;
    offer.acquired.store(true, Ordering::Relaxed);
    ctx.return_efun_result(ctx.txn().nil());
    Ok(())
}
