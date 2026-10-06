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
        let (txn, live) = self.begin().await;
        let handle = self.connect_in(&txn, host, port);
        self.commit(&txn, live).await;
        handle
    }
    async fn write(&self, handle: SocketHandle, data: &'static [u8]) {
        let (txn, live) = self.begin().await;
        assert!(
            self.access(&txn)
                .write(&self.owner, handle, Bytes::from_static(data))
                .unwrap()
        );
        self.commit(&txn, live).await;
    }
    async fn close(&self, handle: SocketHandle) {
        let (txn, live) = self.begin().await;
        self.access(&txn).close(&self.owner, handle).unwrap();
        self.commit(&txn, live).await;
    }
    async fn info(&self, handle: SocketHandle) -> Option<super::super::SocketInfo> {
        let (txn, _live) = self.begin().await;
        self.access(&txn).info(handle)
    }
    fn simulated(&self, backend: Simulated) -> SocketTransport {
        SocketTransport::spawn(&self.gs, self.gs.sockets.take_commands().unwrap(), backend)
    }
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
    };
    let (_writes_tx, writes) = flume::unbounded();
    let (_close_tx, close) = watch::channel(None);
    run_worker(
        handle,
        SocketTarget::Connect {
            host: "test".into(),
            port: 9,
        },
        Arc::new(backend),
        publisher,
        writes,
        close,
        Duration::from_secs(1),
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
        applied: false,
    };
    let (result, stats) = runner.run(&mut publication).await;
    result.unwrap();
    assert_eq!(stats.conflicts, 2);
    assert!(publication.applied);
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
        applied: false,
    };
    let mut live = publication
        .begin_attempt(&f.gs.committer_tx)
        .await
        .unwrap()
        .unwrap();
    assert!(publication.applied);
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
    assert!(!publication.applied);
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
        matches!(&commands.recv_async().await.unwrap()[..], [SocketCommand::Close { handle: h, drain: false }] if *h == handle)
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
