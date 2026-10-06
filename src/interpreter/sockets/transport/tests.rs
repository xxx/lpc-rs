use std::{
    collections::VecDeque,
    pin::Pin,
    sync::atomic::{AtomicUsize, Ordering},
    task::{Context, Poll, Waker},
};

use lpc_rs_utils::config::ConfigBuilder;
use parking_lot::Mutex;
use tokio::io::AsyncReadExt;

use super::*;
use crate::interpreter::{
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
    process::Process,
};

struct Fixture {
    gs: Arc<GlobalState>,
    owner: Arc<Process>,
}

impl Fixture {
    fn new() -> Self {
        let config = ConfigBuilder::default()
            .socket_connect_timeout(100)
            .socket_close_timeout(100)
            .build()
            .unwrap();
        Self::with_config(config)
    }
    fn with_config(config: lpc_rs_utils::config::Config) -> Self {
        let (tx, _) = tokio::sync::mpsc::channel(8);
        Self {
            gs: Arc::new(GlobalState::new(config, tx)),
            owner: Arc::default(),
        }
    }
    async fn begin(&self) -> (TxnHandle, LiveSnapshot) {
        let live = start_txn(&self.gs.committer_tx).await.unwrap();
        (TxnHandle::new(Transaction::new(live.inner.clone())), live)
    }
    fn access<'a>(&'a self, txn: &TxnHandle) -> SocketAccess<'a> {
        SocketAccess {
            sockets: &self.gs.sockets,
            txn: txn.clone(),
        }
    }
    async fn commit(&self, txn: &TxnHandle, mut live: LiveSnapshot) {
        live.disarm();
        commit_changeset(&self.gs.committer_tx, txn.with(Transaction::take_changeset))
            .await
            .unwrap()
            .unwrap();
    }
    fn connect_in(&self, txn: &TxnHandle, host: &str, port: u16) -> SocketHandle {
        let callback = Arc::new(
            FunctionPtrBuilder::default()
                .owner(Arc::downgrade(&self.owner))
                .address(FunctionAddress::Efun(ustr::ustr("dump")))
                .build()
                .unwrap(),
        );
        self.access(txn)
            .connect(&self.owner, host.into(), port, callback)
            .unwrap()
    }
    async fn connect(&self, host: &str, port: u16) -> SocketHandle {
        self.transact(|access| self.connect_in(&access.txn, host, port))
            .await
    }
    async fn write(&self, handle: SocketHandle, data: &'static [u8]) {
        self.transact(|access| {
            assert!(
                access
                    .write(&self.owner, handle, Bytes::from_static(data))
                    .unwrap()
            )
        })
        .await;
    }
    async fn transact<T>(&self, mut action: impl FnMut(&SocketAccess<'_>) -> T) -> T {
        for _ in 0..100 {
            let (txn, mut live) = self.begin().await;
            let result = action(&self.access(&txn));
            live.disarm();
            if commit_changeset(&self.gs.committer_tx, txn.with(Transaction::take_changeset))
                .await
                .unwrap()
                .is_ok()
            {
                return result;
            }
            tokio::task::yield_now().await;
        }
        panic!("test transaction kept conflicting");
    }
    async fn close(&self, handle: SocketHandle) {
        self.transact(|access| access.close(&self.owner, handle).unwrap())
            .await;
    }
    async fn info(&self, handle: SocketHandle) -> Option<super::super::SocketInfo> {
        let (txn, _live) = self.begin().await;
        self.access(&txn).info(handle)
    }
    async fn settle(&self, handle: SocketHandle, succeeded: bool) -> super::super::SocketEvent {
        let mut expected = None;
        self.transact(|access| {
            let lease = access.lease(handle).unwrap().unwrap();
            if let Some(event) = &expected {
                assert_eq!(event, lease.event());
            }
            expected = Some(lease.event().clone());
            assert!(access.acknowledge(&lease, succeeded).unwrap());
            lease.event().clone()
        })
        .await
    }
    fn simulated(&self, backend: Simulated) -> SocketTransport {
        SocketTransport::spawn(&self.gs, self.gs.sockets.take_commands().unwrap(), backend)
    }
    async fn wait_disposed(&self, handle: SocketHandle) {
        let mut commits = self.gs.attempt_runner.commit_watch().unwrap();
        timeout(Duration::from_secs(3), async {
            loop {
                commits.borrow_and_update();
                let present = {
                    let (txn, _live) = self.begin().await;
                    txn.with(|t| self.gs.sockets.cells(t, handle).is_some())
                };
                if !present {
                    return;
                }
                commits.changed().await.unwrap();
            }
        })
        .await
        .unwrap();
    }
}

#[tokio::test]
async fn starting_transport_disposes_retired_queued_output_before_releasing_quota() {
    let f = Fixture::with_config(ConfigBuilder::default().max_sockets(1).build().unwrap());
    let (txn, live) = f.begin().await;
    let handle = f.connect_in(&txn, "test", 9);
    f.access(&txn)
        .write(&f.owner, handle, Bytes::copy_from_slice(b"buffer"))
        .unwrap();
    txn.with(|t| f.gs.sockets.retire_object(t, &f.owner));
    f.commit(&txn, live).await;
    let (txn, _live) = f.begin().await;
    assert!(
        f.access(&txn)
            .connect(
                &f.owner,
                "test".into(),
                9,
                Arc::new(
                    FunctionPtrBuilder::default()
                        .owner(Arc::downgrade(&f.owner))
                        .address(FunctionAddress::Efun(ustr::ustr("dump")))
                        .build()
                        .unwrap()
                )
            )
            .is_err()
    );
    drop(txn);
    drop(_live);
    let script = Script::new([]);
    let (backend, opened) = Simulated::single(&script);
    let transport = f.simulated(backend);
    f.wait_disposed(handle).await;
    assert!(opened.is_empty());
    assert!(script.bytes().is_empty());
    let next = f.connect("test", 9).await;
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Ready { handle, .. } if handle == next)
    );
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn retiring_an_unattached_child_keeps_quota_until_its_descriptor_is_dropped() {
    let f = Fixture::with_config(ConfigBuilder::default().max_sockets(2).build().unwrap());
    let commands = f.gs.sockets.take_commands().unwrap();
    let callback = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&f.owner))
            .address(FunctionAddress::Efun(ustr::ustr("dump")))
            .build()
            .unwrap(),
    );
    let (txn, live) = f.begin().await;
    let listener = f
        .access(&txn)
        .listen(&f.owner, endpoint(0), callback)
        .unwrap();
    f.access(&txn)
        .ready(listener, endpoint(1234), None)
        .unwrap();
    f.commit(&txn, live).await;
    commands.recv().unwrap();
    f.settle(listener, true).await;
    let permit = f
        .transact(|access| {
            let SocketAdmission::Admitted(permit) = access.reserve_accept(listener).unwrap() else {
                panic!("permit");
            };
            permit
        })
        .await;
    f.transact(|access| {
        access
            .accepted(permit, endpoint(1234), endpoint(9))
            .unwrap()
    })
    .await;
    let (events, _events_rx) = flume::bounded(2);
    let mut routing = Routing {
        io: Arc::new(NativeIo {
            backend: Arc::new(TokioBackend),
            publisher: Publisher::new(&f.gs, events),
            connect_timeout: Duration::from_secs(1),
            accepted: flume::bounded(2).0,
        }),
        close_timeout: Duration::from_secs(1),
        workers: HashMap::new(),
        tasks: JoinSet::new(),
        disposals: JoinSet::new(),
    };
    routing.batch(commands.recv().unwrap());
    f.settle(listener, false).await;
    routing.batch(commands.recv().unwrap());
    let finished = timeout(Duration::from_secs(3), routing.tasks.join_next())
        .await
        .unwrap()
        .unwrap()
        .unwrap();
    routing.workers.get_mut(&finished).unwrap().finished = true;
    routing.settle(finished);
    assert!(routing.disposals.is_empty());
    f.transact(|access| {
        assert_eq!(
            access.reserve_accept(listener).unwrap(),
            SocketAdmission::Full
        )
    })
    .await;
    let physical_listener = TcpListener::bind(endpoint(0)).await.unwrap();
    let mut peer = TcpStream::connect(physical_listener.local_addr().unwrap())
        .await
        .unwrap();
    let (writer, remote) = physical_listener.accept().await.unwrap();
    let local = writer.local_addr().unwrap();
    let resource = Resource::Stream {
        writer,
        local,
        remote,
    };
    let _ = routing
        .workers
        .get_mut(&finished)
        .unwrap()
        .resource
        .take()
        .unwrap()
        .send(resource);
    routing.settle(finished);
    timeout(Duration::from_secs(3), routing.disposals.join_next())
        .await
        .unwrap()
        .unwrap()
        .unwrap();
    f.wait_disposed(permit.child).await;
    let mut byte = [0];
    assert_eq!(
        timeout(Duration::from_secs(3), peer.read(&mut byte))
            .await
            .unwrap()
            .unwrap(),
        0
    );
    assert!(matches!(
        f.transact(|access| access.reserve_accept(listener).unwrap())
            .await,
        SocketAdmission::Admitted(_)
    ));
}

async fn event(transport: &SocketTransport) -> SocketIoEvent {
    timeout(Duration::from_secs(3), transport.next_event())
        .await
        .unwrap()
        .unwrap()
}

fn endpoint(port: u16) -> SocketAddr {
    SocketAddr::from(([127, 0, 0, 1], port))
}

enum Action {
    Short(usize),
    Pending,
    Fail,
    Zero,
}

struct ScriptState {
    actions: VecDeque<Action>,
    bytes: Vec<u8>,
    waker: Option<Waker>,
    read_error: bool,
}

struct Script {
    state: Mutex<ScriptState>,
    polls: AtomicUsize,
    dropped: AtomicUsize,
    polled_tx: flume::Sender<()>,
    polled_rx: flume::Receiver<()>,
}

impl Script {
    fn new(actions: impl IntoIterator<Item = Action>) -> Arc<Self> {
        let (polled_tx, polled_rx) = flume::unbounded();
        Arc::new(Self {
            state: Mutex::new(ScriptState {
                actions: actions.into_iter().collect(),
                bytes: Vec::new(),
                waker: None,
                read_error: false,
            }),
            polls: AtomicUsize::new(0),
            dropped: AtomicUsize::new(0),
            polled_tx,
            polled_rx,
        })
    }
    async fn wait_polls(&self, n: usize) {
        timeout(Duration::from_secs(3), async {
            while self.polls.load(Ordering::SeqCst) < n {
                self.polled_rx.recv_async().await.unwrap();
            }
        })
        .await
        .unwrap();
    }
    async fn wait_dropped(&self) {
        timeout(Duration::from_secs(3), async {
            while self.dropped.load(Ordering::SeqCst) == 0 {
                self.polled_rx.recv_async().await.unwrap();
            }
        })
        .await
        .unwrap();
    }
    fn resume(&self) {
        let waker = {
            let mut state = self.state.lock();
            assert!(matches!(state.actions.pop_front(), Some(Action::Pending)));
            state.waker.take()
        };
        if let Some(waker) = waker {
            waker.wake();
        }
    }
    fn bytes(&self) -> Vec<u8> {
        self.state.lock().bytes.clone()
    }
}

struct ScriptWriter(Arc<Script>);

impl Drop for ScriptWriter {
    fn drop(&mut self) {
        self.0.dropped.fetch_add(1, Ordering::SeqCst);
        let _ = self.0.polled_tx.send(());
    }
}

impl AsyncRead for ScriptWriter {
    fn poll_read(
        self: Pin<&mut Self>,
        _: &mut Context<'_>,
        _: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        if self.0.state.lock().read_error {
            Poll::Ready(Err(io::Error::new(
                io::ErrorKind::ConnectionReset,
                "scripted read failure",
            )))
        } else {
            Poll::Pending
        }
    }
}

impl AsyncWrite for ScriptWriter {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        bytes: &[u8],
    ) -> Poll<io::Result<usize>> {
        let mut state = self.0.state.lock();
        self.0.polls.fetch_add(1, Ordering::SeqCst);
        self.0.polled_tx.send(()).unwrap();
        if matches!(state.actions.front(), Some(Action::Pending)) {
            state.waker = Some(cx.waker().clone());
            return Poll::Pending;
        }
        match state.actions.pop_front() {
            Some(Action::Fail) => Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "scripted write failure",
            ))),
            Some(Action::Zero) => Poll::Ready(Ok(0)),
            action => {
                let count = if let Some(Action::Short(n)) = action {
                    n.min(bytes.len())
                } else {
                    bytes.len()
                };
                state.bytes.extend_from_slice(&bytes[..count]);
                Poll::Ready(Ok(count))
            }
        }
    }
    fn poll_flush(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }
    fn poll_shutdown(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }
}

struct Simulated {
    scripts: HashMap<String, Arc<Script>>,
    gates: HashMap<String, Arc<tokio::sync::Semaphore>>,
    opened: flume::Sender<String>,
}

impl Simulated {
    fn single(script: &Arc<Script>) -> (Self, flume::Receiver<String>) {
        let (opened, rx) = flume::unbounded();
        (
            Self {
                scripts: HashMap::from([("test".into(), script.clone())]),
                gates: HashMap::new(),
                opened,
            },
            rx,
        )
    }
}

impl Backend for Simulated {
    type Writer = ScriptWriter;
    async fn open(&self, target: SocketTarget) -> io::Result<Resource<Self::Writer>> {
        let SocketTarget::Connect { host, .. } = target else {
            panic!("simulation only opens streams")
        };
        let _ = self.opened.send(host.clone());
        if let Some(gate) = self.gates.get(&host) {
            gate.acquire().await.unwrap().forget();
        }
        let script = self.scripts.get(&host).ok_or_else(|| {
            io::Error::new(io::ErrorKind::ConnectionRefused, "scripted connect failure")
        })?;
        Ok(Resource::Stream {
            writer: ScriptWriter(script.clone()),
            local: endpoint(1234),
            remote: endpoint(9),
        })
    }
    async fn accept(&self, _: &TcpListener) -> io::Result<Resource<Self::Writer>> {
        unreachable!("simulation does not open listeners")
    }
}

#[tokio::test]
async fn partial_writes_complete_one_buffer_before_the_next_without_replaying_a_prefix() {
    let f = Fixture::new();
    let script = Script::new([Action::Short(2), Action::Pending, Action::Short(1)]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let (txn, live) = f.begin().await;
    let handle = f.connect_in(&txn, "test", 9);
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"a\0bcd"))
        .unwrap();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"EF"))
        .unwrap();
    f.commit(&txn, live).await;
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Ready { handle: h, .. } if h == handle)
    );
    script.wait_polls(2).await;
    assert_eq!(script.bytes(), b"a\0");
    assert_eq!(f.info(handle).await.unwrap().output_pending, 7);
    assert!(transport.events.is_empty());
    script.resume();
    assert_eq!(
        event(&transport).await,
        SocketIoEvent::WriteComplete {
            handle,
            sequence: 1
        }
    );
    assert_eq!(
        event(&transport).await,
        SocketIoEvent::WriteComplete {
            handle,
            sequence: 2
        }
    );
    assert_eq!(script.bytes(), b"a\0bcdEF");
    assert_eq!(f.info(handle).await.unwrap().output_pending, 0);
    transport.shutdown().await.unwrap();
    assert_eq!(script.dropped.load(Ordering::SeqCst), 1);
}

#[tokio::test]
async fn an_error_after_a_partial_write_discards_the_remaining_queue_and_preserves_the_prefix_once()
{
    let f = Fixture::new();
    let script = Script::new([Action::Short(2), Action::Fail]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let (txn, live) = f.begin().await;
    let handle = f.connect_in(&txn, "test", 9);
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"abcdef"))
        .unwrap();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"never"))
        .unwrap();
    f.commit(&txn, live).await;
    event(&transport).await;
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed {
            reason: SocketCloseReason::Error,
            operation: Some(SocketOperation::Write),
            ..
        }
    ));
    assert_eq!(script.bytes(), b"ab");
    let info = f.info(handle).await.unwrap();
    assert_eq!(info.state, SocketState::Closed);
    assert_eq!(info.output_pending, 0);
    assert_eq!(script.dropped.load(Ordering::SeqCst), 1);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn zero_length_os_write_is_terminal_instead_of_spinning() {
    let f = Fixture::new();
    let script = Script::new([Action::Zero]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    event(&transport).await;
    f.write(handle, b"abc").await;
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed {
            reason: SocketCloseReason::Error,
            operation: Some(SocketOperation::Write),
            ..
        }
    ));
    assert_eq!(script.polls.load(Ordering::SeqCst), 1);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn graceful_close_finishes_all_admitted_buffers_in_order() {
    let f = Fixture::new();
    let script = Script::new([Action::Short(1), Action::Pending]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    event(&transport).await;
    f.write(handle, b"abc").await;
    script.wait_polls(2).await;
    f.write(handle, b"DEF").await;
    f.close(handle).await;
    script.resume();
    assert_eq!(
        event(&transport).await,
        SocketIoEvent::WriteComplete {
            handle,
            sequence: 1
        }
    );
    assert_eq!(
        event(&transport).await,
        SocketIoEvent::WriteComplete {
            handle,
            sequence: 2
        }
    );
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed {
            reason: SocketCloseReason::Local,
            operation: None,
            error: None,
            ..
        }
    ));
    assert_eq!(script.bytes(), b"abcDEF");
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn close_timeout_stops_a_blocked_partial_write() {
    let f = Fixture::new();
    let script = Script::new([Action::Short(1), Action::Pending]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    event(&transport).await;
    f.write(handle, b"abc").await;
    script.wait_polls(2).await;
    f.close(handle).await;
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed {
            reason: SocketCloseReason::Timeout,
            operation: Some(SocketOperation::Close),
            ..
        }
    ));
    assert_eq!(script.bytes(), b"a");
    assert_eq!(f.info(handle).await.unwrap().output_pending, 0);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn owner_retirement_interrupts_a_blocked_write_without_draining() {
    let f = Fixture::new();
    let script = Script::new([Action::Short(1), Action::Pending]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    event(&transport).await;
    f.write(handle, b"abc").await;
    script.wait_polls(2).await;
    let (txn, live) = f.begin().await;
    txn.with(|txn| f.gs.sockets.retire_object(txn, &f.owner));
    f.commit(&txn, live).await;
    script.wait_dropped().await;
    assert_eq!(script.bytes(), b"a");
    assert_eq!(script.dropped.load(Ordering::SeqCst), 1);
    assert!(f.info(handle).await.is_none());
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn connect_timeout_discards_connecting_output_and_publishes_terminal_state() {
    let f = Fixture::new();
    let script = Script::new([]);
    let (mut backend, opened) = Simulated::single(&script);
    backend
        .gates
        .insert("test".into(), Arc::new(tokio::sync::Semaphore::new(0)));
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    f.write(handle, b"never").await;
    opened.recv_async().await.unwrap();
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed {
            reason: SocketCloseReason::Timeout,
            operation: Some(SocketOperation::Connect),
            ..
        }
    ));
    assert_eq!(script.bytes(), b"");
    assert_eq!(f.info(handle).await.unwrap().output_pending, 0);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn close_winning_connect_completion_suppresses_ready_and_queued_bytes() {
    let f = Fixture::new();
    let script = Script::new([]);
    let (mut backend, opened) = Simulated::single(&script);
    let gate = Arc::new(tokio::sync::Semaphore::new(0));
    backend.gates.insert("test".into(), gate.clone());
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    f.write(handle, b"never").await;
    opened.recv_async().await.unwrap();
    f.close(handle).await;
    gate.add_permits(1);
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed {
            reason: SocketCloseReason::Local,
            ..
        }
    ));
    assert_eq!(script.bytes(), b"");
    assert_eq!(f.info(handle).await.unwrap().state, SocketState::Closed);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn already_retired_open_commands_never_start_os_setup() {
    let f = Fixture::new();
    let (txn, live) = f.begin().await;
    let handle = f.connect_in(&txn, "test", 9);
    txn.with(|txn| f.gs.sockets.retire_object(txn, &f.owner));
    f.commit(&txn, live).await;
    let script = Script::new([]);
    let (backend, opened) = Simulated::single(&script);
    let (events, _) = flume::bounded(1);
    let publisher = Publisher {
        gs: Arc::downgrade(&f.gs),
        events,
        commits: f.gs.attempt_runner.commit_watch(),
    };
    let (_writes_tx, writes) = flume::unbounded();
    let (_close_tx, close) = watch::channel(None);
    run_worker(
        handle,
        SocketTarget::Connect {
            host: "test".into(),
            port: 9,
        },
        Arc::new(NativeIo {
            backend: Arc::new(backend),
            publisher,
            connect_timeout: Duration::from_secs(1),
            accepted: flume::unbounded().0,
        }),
        writes,
        close,
    )
    .await;
    assert!(opened.is_empty());
    assert!(f.info(handle).await.is_none());
}

#[tokio::test]
async fn one_slow_connect_does_not_block_another_stream() {
    let f = Fixture::new();
    let slow = Script::new([]);
    let fast = Script::new([]);
    let (mut backend, opened) = Simulated::single(&slow);
    backend.scripts.insert("fast".into(), fast.clone());
    backend
        .gates
        .insert("test".into(), Arc::new(tokio::sync::Semaphore::new(0)));
    let transport = f.simulated(backend);
    f.connect("test", 9).await;
    opened.recv_async().await.unwrap();
    let handle = f.connect("fast", 9).await;
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Ready { handle: h, .. } if h == handle)
    );
    f.write(handle, b"ok").await;
    assert_eq!(
        event(&transport).await,
        SocketIoEvent::WriteComplete {
            handle,
            sequence: 1
        }
    );
    assert_eq!(fast.bytes(), b"ok");
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn failed_connect_is_published_without_a_ready_event() {
    let f = Fixture::new();
    let script = Script::new([]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let handle = f.connect("missing", 9).await;
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Closed { handle: h, reason: SocketCloseReason::Error, operation: Some(SocketOperation::Connect), .. } if h == handle)
    );
    assert_eq!(f.info(handle).await.unwrap().state, SocketState::Closed);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn native_completion_retries_do_not_repeat_an_os_write() {
    let f = Fixture::new();
    let handle = f.connect("test", 9).await;
    let (txn, live) = f.begin().await;
    f.access(&txn)
        .ready(handle, endpoint(1234), Some(endpoint(9)))
        .unwrap();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"abc"))
        .unwrap();
    f.commit(&txn, live).await;
    let script = Script::new([Action::Short(1)]);
    let mut writer = ScriptWriter(script.clone());
    writer.write_all(b"abc").await.unwrap();

    let (proxy_tx, proxy_rx) = flume::unbounded();
    let actual = f.gs.committer_tx.clone();
    let proxy = tokio::spawn(async move {
        let mut remaining = 2;
        while let Ok(message) = proxy_rx.recv_async().await {
            match message {
                CommitProtocol::Commit {
                    changeset,
                    releases_base,
                    reply,
                } if remaining > 0 => {
                    remaining -= 1;
                    if releases_base {
                        actual
                            .send(CommitProtocol::Drop(changeset.base_version()))
                            .unwrap();
                    }
                    reply.send(Err(Conflict::Forced)).unwrap();
                }
                message => actual.send(message).unwrap(),
            }
        }
    });
    let runner = crate::interpreter::stm::AttemptRunner::new(proxy_tx, None);
    let mut publication = Publication {
        gs: f.gs.clone(),
        update: Update::Event(SocketIoEvent::WriteComplete {
            handle,
            sequence: 1,
        }),
        txn: None,
        outcome: Outcome::Applied(false),
    };
    let (result, stats) = runner.run(&mut publication).await;
    result.unwrap();
    assert_eq!(stats.conflicts, 2);
    assert!(matches!(publication.outcome, Outcome::Applied(true)));
    assert_eq!(script.bytes(), b"abc");
    assert_eq!(script.polls.load(Ordering::SeqCst), 2);
    assert_eq!(f.info(handle).await.unwrap().output_pending, 0);
    assert_eq!(f.gs.committer_stats().await.unwrap().live_snapshots, 0);
    proxy.abort();
}

#[tokio::test]
async fn close_invalidates_an_in_flight_ready_publication() {
    let f = Fixture::new();
    let handle = f.connect("test", 9).await;
    f.write(handle, b"never").await;
    let mut publication = Publication {
        gs: f.gs.clone(),
        update: Update::Event(SocketIoEvent::Ready {
            handle,
            local: endpoint(1234),
            remote: Some(endpoint(9)),
        }),
        txn: None,
        outcome: Outcome::Applied(false),
    };
    let mut live = publication
        .begin_attempt(&f.gs.committer_tx)
        .await
        .unwrap()
        .unwrap();
    assert!(matches!(publication.outcome, Outcome::Applied(true)));
    f.close(handle).await;
    live.disarm();
    assert!(
        publication
            .commit_phase(&f.gs.committer_tx, live)
            .await
            .unwrap()
            .0
            .is_err()
    );
    f.gs.attempt_runner.run(&mut publication).await.0.unwrap();
    assert!(matches!(publication.outcome, Outcome::Applied(false)));
    assert_eq!(f.info(handle).await.unwrap().state, SocketState::Closing);
}

#[tokio::test]
async fn real_tcp_connect_writes_binary_output_and_gracefully_closes() {
    let f = Fixture::new();
    let listener = TcpListener::bind(endpoint(0)).await.unwrap();
    let address = listener.local_addr().unwrap();
    let transport = SocketTransport::start(&f.gs).unwrap();
    assert!(SocketTransport::start(&f.gs).is_err());
    let handle = f.connect("localhost", address.port()).await;
    let (mut peer, _) = listener.accept().await.unwrap();
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Ready { remote: Some(remote), .. } if remote == address)
    );
    f.write(handle, b"a\0b\xff").await;
    event(&transport).await;
    f.close(handle).await;
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed {
            reason: SocketCloseReason::Local,
            ..
        }
    ));
    let mut data = Vec::new();
    peer.read_to_end(&mut data).await.unwrap();
    assert_eq!(data, b"a\0b\xff");
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn real_tcp_listener_publishes_its_actual_ephemeral_endpoint_and_releases_it_on_close() {
    let f = Fixture::new();
    let transport = SocketTransport::start(&f.gs).unwrap();
    let (txn, live) = f.begin().await;
    let callback = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&f.owner))
            .address(FunctionAddress::Efun(ustr::ustr("dump")))
            .build()
            .unwrap(),
    );
    let handle = f
        .access(&txn)
        .listen(&f.owner, endpoint(0), callback)
        .unwrap();
    f.commit(&txn, live).await;
    let SocketIoEvent::Ready {
        local,
        remote: None,
        ..
    } = event(&transport).await
    else {
        panic!("listener was not ready")
    };
    assert_ne!(local.port(), 0);
    assert_eq!(f.info(handle).await.unwrap().local, Some(local));
    f.close(handle).await;
    event(&transport).await;
    let rebound = TcpListener::bind(local).await.unwrap();
    drop(rebound);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn closing_before_transport_start_publishes_closed_without_os_setup() {
    let f = Fixture::new();
    let handle = f.connect("test", 9).await;
    f.close(handle).await;
    let script = Script::new([]);
    let (backend, opened) = Simulated::single(&script);
    let transport = f.simulated(backend);
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Closed { handle: h, reason: SocketCloseReason::Local, .. } if h == handle)
    );
    assert!(opened.is_empty());
    assert_eq!(f.info(handle).await.unwrap().state, SocketState::Closed);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn aborted_writes_never_reach_the_os_writer() {
    let f = Fixture::new();
    let script = Script::new([]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    event(&transport).await;
    {
        let (txn, _live) = f.begin().await;
        f.access(&txn)
            .write(&f.owner, handle, Bytes::from_static(b"never"))
            .unwrap();
    }
    f.write(handle, b"ok").await;
    assert_eq!(
        event(&transport).await,
        SocketIoEvent::WriteComplete {
            handle,
            sequence: 1
        }
    );
    assert_eq!(script.bytes(), b"ok");
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn dropping_the_vm_stops_workers_without_extending_its_lifetime() {
    let f = Fixture::new();
    let script = Script::new([Action::Pending]);
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    event(&transport).await;
    f.write(handle, b"never").await;
    script.wait_polls(1).await;
    let weak = Arc::downgrade(&f.gs);
    drop(f);
    script.wait_dropped().await;
    assert!(weak.upgrade().is_none());
    assert!(
        timeout(Duration::from_secs(3), transport.next_event())
            .await
            .unwrap()
            .is_none()
    );
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn retiring_a_closed_record_cancels_a_backpressured_terminal_notification() {
    let f = Fixture::new();
    let handle = f.connect("test", 9).await;
    let commands = f.gs.sockets.take_commands().unwrap();
    commands.recv_async().await.unwrap();
    let (txn, live) = f.begin().await;
    f.access(&txn).closed(handle);
    f.commit(&txn, live).await;
    let (events, received) = flume::bounded(1);
    events
        .send(SocketIoEvent::Ready {
            handle,
            local: endpoint(1234),
            remote: Some(endpoint(9)),
        })
        .unwrap();
    let publisher = Publisher {
        gs: Arc::downgrade(&f.gs),
        events,
        commits: f.gs.attempt_runner.commit_watch(),
    };
    let (close_tx, mut close) = watch::channel(None);
    let terminal = SocketIoEvent::Closed {
        handle,
        reason: SocketCloseReason::Error,
        operation: Some(SocketOperation::Write),
        error: Some("failed".into()),
        local: Some(endpoint(1234)),
        remote: Some(endpoint(9)),
    };
    let pending = tokio::spawn(async move {
        publisher.emit_terminal(terminal, &mut close).await;
    });
    let (txn, live) = f.begin().await;
    txn.with(|txn| f.gs.sockets.retire_object(txn, &f.owner));
    f.commit(&txn, live).await;
    assert!(
        matches!(&commands.recv_async().await.unwrap()[..], [SocketCommand::Retire { handle: h }] if *h == handle)
    );
    close_tx
        .send(Some(Close {
            drain: false,
            deadline: Instant::now(),
        }))
        .unwrap();
    timeout(Duration::from_secs(3), pending)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(received.len(), 1);
    assert!(f.info(handle).await.is_none());
}

#[tokio::test]
async fn real_bind_failure_publishes_a_listen_error() {
    let f = Fixture::new();
    let occupied = TcpListener::bind(endpoint(0)).await.unwrap();
    let transport = SocketTransport::start(&f.gs).unwrap();
    let (txn, live) = f.begin().await;
    let callback = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&f.owner))
            .address(FunctionAddress::Efun(ustr::ustr("dump")))
            .build()
            .unwrap(),
    );
    let handle = f
        .access(&txn)
        .listen(&f.owner, occupied.local_addr().unwrap(), callback)
        .unwrap();
    f.commit(&txn, live).await;
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Closed { handle: h, reason: SocketCloseReason::Error, operation: Some(SocketOperation::Listen), .. } if h == handle)
    );
    assert_eq!(f.info(handle).await.unwrap().state, SocketState::Closed);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn disconnecting_the_worker_control_channel_stops_an_idle_writer() {
    let script = Script::new([]);
    let mut writer = ScriptWriter(script.clone());
    let (events, _) = flume::bounded(1);
    let publisher = Publisher {
        gs: Weak::new(),
        events,
        commits: None,
    };
    let (_writes_tx, writes) = flume::unbounded();
    let (close_tx, mut close) = watch::channel(None);
    drop(close_tx);
    let terminal = stream(
        SocketHandle::try_from(1).unwrap(),
        &mut writer,
        &publisher,
        writes,
        &mut close,
    )
    .await;
    assert_eq!(terminal.reason, SocketCloseReason::Local);
    assert_eq!(script.polls.load(Ordering::SeqCst), 0);
}

async fn connected(input: usize) -> (Fixture, SocketTransport, SocketHandle, TcpStream) {
    let f = Fixture::with_config(
        ConfigBuilder::default()
            .max_pending_socket_input(input)
            .build()
            .unwrap(),
    );
    let listener = TcpListener::bind(endpoint(0)).await.unwrap();
    let transport = SocketTransport::start(&f.gs).unwrap();
    let handle = f
        .connect("127.0.0.1", listener.local_addr().unwrap().port())
        .await;
    let (peer, _) = timeout(Duration::from_secs(3), listener.accept())
        .await
        .unwrap()
        .unwrap();
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Ready { .. }
    ));
    assert!(matches!(
        f.settle(handle, true).await.data,
        SocketEventData::Ready { .. }
    ));
    (f, transport, handle, peer)
}

async fn listening(accepts: usize) -> (Fixture, SocketTransport, SocketHandle, SocketAddr) {
    let f = Fixture::with_config(
        ConfigBuilder::default()
            .max_pending_socket_accepts(accepts)
            .build()
            .unwrap(),
    );
    let transport = SocketTransport::start(&f.gs).unwrap();
    let (txn, live) = f.begin().await;
    let callback = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&f.owner))
            .address(FunctionAddress::Efun(ustr::ustr("dump")))
            .build()
            .unwrap(),
    );
    let handle = f
        .access(&txn)
        .listen(&f.owner, endpoint(0), callback)
        .unwrap();
    f.commit(&txn, live).await;
    let SocketIoEvent::Ready {
        local,
        remote: None,
        ..
    } = event(&transport).await
    else {
        panic!()
    };
    f.settle(handle, true).await;
    (f, transport, handle, local)
}

#[tokio::test]
async fn bounded_real_input_survives_aborted_delivery_and_resumes_after_commit() {
    let (f, transport, handle, mut peer) = connected(3).await;
    peer.write_all(b"a\0bcde").await.unwrap();
    let mut received = Vec::new();
    while received.len() < 3 {
        let SocketIoEvent::Read { data, .. } = event(&transport).await else {
            panic!()
        };
        received.extend_from_slice(&data);
    }
    assert_eq!(received, b"a\0b");
    assert!(
        timeout(Duration::from_millis(50), transport.next_event())
            .await
            .is_err()
    );
    let (attempt, live) = f.begin().await;
    let lease = f.access(&attempt).lease(handle).unwrap().unwrap();
    assert!(f.access(&attempt).acknowledge(&lease, true).unwrap());
    drop(live);
    drop(attempt);
    let (retry, live) = f.begin().await;
    assert_eq!(f.access(&retry).input_capacity(handle).unwrap(), Some(0));
    let same = f.access(&retry).lease(handle).unwrap().unwrap();
    assert_eq!(same.event(), lease.event());
    f.access(&retry).acknowledge(&same, true).unwrap();
    f.commit(&retry, live).await;
    while received.len() < 6 {
        let SocketIoEvent::Read { data, .. } = event(&transport).await else {
            panic!()
        };
        received.extend_from_slice(&data);
        f.settle(handle, true).await;
    }
    assert_eq!(received, b"a\0bcde");
    loop {
        let (txn, live) = f.begin().await;
        let Some(lease) = f.access(&txn).lease(handle).unwrap() else {
            break;
        };
        f.access(&txn).acknowledge(&lease, true).unwrap();
        f.commit(&txn, live).await;
    }
    let (txn, _live) = f.begin().await;
    assert_eq!(f.access(&txn).input_capacity(handle).unwrap(), Some(3));
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn remote_half_close_delivers_eof_once_and_still_accepts_a_binary_response() {
    let (f, transport, handle, mut peer) = connected(16).await;
    peer.write_all(b"request\0").await.unwrap();
    peer.shutdown().await.unwrap();
    let mut received = Vec::new();
    loop {
        match event(&transport).await {
            SocketIoEvent::Read { data, .. } => {
                received.extend_from_slice(&data);
                assert!(matches!(
                    f.settle(handle, true).await.data,
                    SocketEventData::Read(_)
                ));
            }
            SocketIoEvent::Eof { handle: found, .. } => {
                assert_eq!(found, handle);
                assert_eq!(f.settle(handle, true).await.data, SocketEventData::Eof);
                break;
            }
            other => panic!("unexpected {other:?}"),
        }
    }
    assert_eq!(received, b"request\0");
    assert!(f.info(handle).await.unwrap().read_eof);
    assert_eq!(f.info(handle).await.unwrap().state, SocketState::Open);
    f.write(handle, b"response\0").await;
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::WriteComplete { .. }
    ));
    assert_eq!(f.settle(handle, true).await.data, SocketEventData::Drained);
    let mut response = [0; 9];
    timeout(Duration::from_secs(3), peer.read_exact(&mut response))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(&response, b"response\0");
    assert!(
        timeout(Duration::from_millis(50), transport.next_event())
            .await
            .is_err()
    );
    f.close(handle).await;
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed { .. }
    ));
    assert!(matches!(
        f.settle(handle, false).await.data,
        SocketEventData::Closed { .. }
    ));
    assert!(f.info(handle).await.is_none());
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn accepted_stream_reads_and_queued_writes_wait_for_committed_accept_delivery() {
    let (f, transport, parent, address) = listening(1).await;
    let mut peer = TcpStream::connect(address).await.unwrap();
    let SocketIoEvent::Accept { handle, child } = event(&transport).await else {
        panic!()
    };
    assert_eq!(handle, parent);
    assert_eq!(f.info(child).await.unwrap().state, SocketState::Open);
    peer.write_all(b"hello").await.unwrap();
    f.write(child, b"reply").await;
    let mut reply = [0; 5];
    assert!(
        timeout(Duration::from_millis(50), peer.read_exact(&mut reply))
            .await
            .is_err()
    );
    assert!(
        timeout(Duration::from_millis(50), transport.next_event())
            .await
            .is_err()
    );
    let (txn, live) = f.begin().await;
    assert!(f.access(&txn).lease(child).unwrap().is_none());
    let lease = f.access(&txn).lease(parent).unwrap().unwrap();
    assert_eq!(lease.event().data, SocketEventData::Accept(child));
    f.access(&txn).acknowledge(&lease, true).unwrap();
    drop(live);
    let (txn, _live) = f.begin().await;
    assert_eq!(f.access(&txn).input_capacity(child).unwrap(), None);
    f.settle(parent, true).await;
    timeout(Duration::from_secs(3), peer.read_exact(&mut reply))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(&reply, b"reply");
    let mut received = Vec::new();
    let mut completed = false;
    while received.len() < 5 || !completed {
        match event(&transport).await {
            SocketIoEvent::Read { handle, data, .. } => {
                assert_eq!(handle, child);
                received.extend_from_slice(&data);
            }
            SocketIoEvent::WriteComplete { handle, .. } => {
                assert_eq!(handle, child);
                completed = true;
            }
            other => panic!("unexpected {other:?}"),
        }
    }
    assert_eq!(received, b"hello");
    f.close(parent).await;
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Closed { handle, .. } if handle == parent)
    );
    f.settle(parent, true).await;
    f.write(child, b"still here").await;
    assert!(
        matches!(event(&transport).await, SocketIoEvent::WriteComplete { handle, .. } if handle == child)
    );
    let mut reply = [0; 10];
    timeout(Duration::from_secs(3), peer.read_exact(&mut reply))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(&reply, b"still here");
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn pending_accept_limit_pauses_os_accepts_and_failed_delivery_releases_capacity() {
    let (f, transport, parent, address) = listening(1).await;
    let mut first = TcpStream::connect(address).await.unwrap();
    let SocketIoEvent::Accept { child, .. } = event(&transport).await else {
        panic!()
    };
    let _second = TcpStream::connect(address).await.unwrap();
    assert!(
        timeout(Duration::from_millis(50), transport.next_event())
            .await
            .is_err()
    );
    assert_eq!(
        f.settle(parent, false).await.data,
        SocketEventData::Accept(child)
    );
    let mut byte = [0];
    assert_eq!(
        timeout(Duration::from_secs(3), first.read(&mut byte))
            .await
            .unwrap()
            .unwrap(),
        0
    );
    assert!(f.info(child).await.is_none());
    let SocketIoEvent::Accept { child: next, .. } = event(&transport).await else {
        panic!()
    };
    assert!(next.get() > child.get());
    assert_eq!(
        f.settle(parent, false).await.data,
        SocketEventData::Accept(next)
    );
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn listener_close_drops_its_undelivered_physical_child_and_reserved_slot() {
    let (f, transport, parent, address) = listening(2).await;
    let mut peer = TcpStream::connect(address).await.unwrap();
    let SocketIoEvent::Accept { child, .. } = event(&transport).await else {
        panic!()
    };
    f.close(parent).await;
    assert!(
        matches!(event(&transport).await, SocketIoEvent::Closed { handle, .. } if handle == parent)
    );
    let mut byte = [0];
    assert_eq!(
        timeout(Duration::from_secs(3), peer.read(&mut byte))
            .await
            .unwrap()
            .unwrap(),
        0
    );
    assert!(f.info(child).await.is_none());
    assert!(matches!(
        f.settle(parent, true).await.data,
        SocketEventData::Closed { .. }
    ));
    let (txn, _live) = f.begin().await;
    assert!(f.access(&txn).query(None).unwrap().is_empty());
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn read_failure_publishes_native_terminal_state_before_failed_callback_cleanup() {
    let f = Fixture::new();
    let script = Script::new([]);
    script.state.lock().read_error = true;
    let (backend, _) = Simulated::single(&script);
    let transport = f.simulated(backend);
    let handle = f.connect("test", 9).await;
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Ready { .. }
    ));
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed {
            operation: Some(SocketOperation::Read),
            reason: SocketCloseReason::Error,
            ..
        }
    ));
    assert_eq!(f.info(handle).await.unwrap().state, SocketState::Closed);
    f.settle(handle, false).await;
    assert!(matches!(
        f.settle(handle, false).await.data,
        SocketEventData::Closed {
            operation: Some(SocketOperation::Read),
            ..
        }
    ));
    assert!(f.info(handle).await.is_none());
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn tiny_reads_do_not_retain_unused_receive_buffer_capacity() {
    let (f, transport, handle, mut peer) = connected(4096).await;
    peer.write_all(b"x").await.unwrap();
    let SocketIoEvent::Read { data, .. } = event(&transport).await else {
        panic!()
    };
    assert_eq!(data.as_ref(), b"x");
    f.settle(handle, true).await;
    f.close(handle).await;
    assert!(matches!(
        event(&transport).await,
        SocketIoEvent::Closed { .. }
    ));
    f.settle(handle, true).await;
    let owned = data
        .try_into_mut()
        .expect("retired input has no inbox or worker references");
    assert_eq!(owned.capacity(), 1);
    transport.shutdown().await.unwrap();
}

#[tokio::test]
async fn listener_close_preserves_an_already_delivered_child_descriptor() {
    let mut f = Fixture::new();
    let actual = f.gs.committer_tx.clone();
    let (proxy_tx, proxy_rx) = flume::unbounded();
    let (committed_tx, committed_rx) = flume::bounded(1);
    let gate = Arc::new(tokio::sync::Semaphore::new(0));
    let proxy_gate = gate.clone();
    let proxy = tokio::spawn(async move {
        while let Ok(message) = proxy_rx.recv_async().await {
            match message {
                CommitProtocol::Commit {
                    changeset,
                    releases_base,
                    reply,
                } if changeset.socket_publications.iter().any(|publication| {
                    publication
                        .commands
                        .iter()
                        .any(|command| matches!(command, SocketCommand::OpenAccepted { .. }))
                }) =>
                {
                    let (response, result) = flume::bounded(1);
                    actual
                        .send(CommitProtocol::Commit {
                            changeset,
                            releases_base,
                            reply: response,
                        })
                        .unwrap();
                    let committed = result.recv_async().await.unwrap();
                    if committed.is_ok() {
                        committed_tx.send(()).unwrap();
                        proxy_gate.acquire().await.unwrap().forget();
                    }
                    reply.send(committed).unwrap();
                }
                message => actual.send(message).unwrap(),
            }
        }
    });
    let watch = f.gs.attempt_runner.commit_watch();
    Arc::get_mut(&mut f.gs)
        .unwrap()
        .set_attempt_runner(Arc::new(crate::interpreter::stm::AttemptRunner::new(
            proxy_tx, watch,
        )));
    let listener = TcpListener::bind(endpoint(0)).await.unwrap();
    let local = listener.local_addr().unwrap();
    let callback = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&f.owner))
            .address(FunctionAddress::Efun(ustr::ustr("dump")))
            .build()
            .unwrap(),
    );
    let (txn, live) = f.begin().await;
    let handle = f.access(&txn).listen(&f.owner, local, callback).unwrap();
    f.access(&txn).ready(handle, local, None).unwrap();
    f.commit(&txn, live).await;
    f.settle(handle, true).await;
    let (events_tx, _events_rx) = flume::bounded(f.gs.config.max_sockets);
    let publisher = Publisher::new(&f.gs, events_tx);
    let (accepted_tx, accepted_rx) = flume::bounded(f.gs.config.max_sockets);
    let (close_tx, mut close) = watch::channel(None);
    let listening = tokio::spawn(async move {
        listen(
            handle,
            listener,
            Arc::new(TokioBackend),
            &publisher,
            &mut close,
            accepted_tx,
        )
        .await
    });
    let mut peer = TcpStream::connect(local).await.unwrap();
    timeout(Duration::from_secs(3), committed_rx.recv_async())
        .await
        .unwrap()
        .unwrap();
    let super::super::SocketEventData::Accept(child) = f.settle(handle, true).await.data else {
        panic!("accept");
    };
    f.close(handle).await;
    close_tx
        .send(Some(Close {
            drain: false,
            deadline: Instant::now() + Duration::from_millis(100),
        }))
        .unwrap();
    gate.add_permits(1);
    timeout(Duration::from_secs(3), listening)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(f.info(child).await.unwrap().state, SocketState::Open);
    let (_, Resource::Stream { mut writer, .. }) = accepted_rx.recv_async().await.unwrap() else {
        panic!("stream");
    };
    writer.write_all(b"x").await.unwrap();
    let mut byte = [0];
    let read = timeout(Duration::from_secs(3), peer.read(&mut byte))
        .await
        .unwrap()
        .unwrap();
    proxy.abort();
    assert_eq!(read, 1);
    assert_eq!(byte, [b'x']);
}
