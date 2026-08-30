use std::{
    net::SocketAddr,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use arc_swap::{ArcSwap, ArcSwapOption};
use flume::Sender as FlumeSender;
use lpc_rs_telnet::{Opt, Session};
use tokio::sync::mpsc::{UnboundedSender, error::SendError};

use crate::{
    interpreter::{function_type::function_ptr::FunctionPtr, process::Process},
    telnet::ops::{BrokerOp, ConnectionOp},
};

/// A struct to encapsulate the state of awaiting a line of input from the user.
#[derive(Debug, Clone)]
pub struct InputTo {
    /// The function to call when we receive input.
    pub ptr: Arc<FunctionPtr>,

    /// Whether `no_echo` was set when `input_to` was called, so we know
    /// that we need to re-enable it.
    pub no_echo: bool,
}

impl PartialEq for InputTo {
    fn eq(&self, other: &Self) -> bool {
        // heh
        std::ptr::eq(&self.ptr, &other.ptr) && self.no_echo == other.no_echo
    }
}

/// What the session has learned about the client; every field always
/// answerable, unknown is zero.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Snapshot {
    /// Window columns; 0 until NAWS reports.
    pub cols: u16,
    /// Window rows; 0 until NAWS reports.
    pub rows: u16,
    /// The charset CHARSET settled on; `None` until it does.
    pub charset: Option<String>,
    /// GMCP is on.
    pub gmcp: bool,
    /// MXP is on.
    pub mxp: bool,
    /// EOR is on.
    pub eor: bool,
}

impl Snapshot {
    /// The session's knowledge right now.
    pub fn of(session: &Session) -> Self {
        let (cols, rows) = session.naws().unwrap_or((0, 0));
        Self {
            cols,
            rows,
            charset: session.charset().map(str::to_owned),
            gmcp: session.is_on(Opt::Gmcp),
            mxp: session.is_on(Opt::Mxp),
            eor: session.is_on(Opt::Eor),
        }
    }
}

/// A connection from a user. The binding module writes its body, the loop
/// its `input_to`; nothing else writes it.
#[derive(Debug)]
pub struct Connection {
    /// The address of the client.
    pub address: SocketAddr,

    /// The body the loop dispatches lines to.
    process: ArcSwapOption<Process>,

    /// The loop's channel.
    tx: UnboundedSender<ConnectionOp>,

    /// The channel we use to send messages to the [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker).
    pub broker_tx: FlumeSender<BrokerOp>,

    /// The function the next line goes to.
    input_to: ArcSwapOption<InputTo>,

    /// The loop's mirror of the session, for readers that never see it.
    snapshot: ArcSwap<Snapshot>,

    /// Set by `detach`; a pending `Effect::Exec` flush checks it.
    dead: AtomicBool,

    /// `logon()` returned non-zero.
    logged_in: AtomicBool,
}

impl Connection {
    /// Creates a new [`Connection`].
    pub fn new(
        address: SocketAddr,
        connection_tx: UnboundedSender<ConnectionOp>,
        broker_tx: FlumeSender<BrokerOp>,
    ) -> Self {
        Self {
            address,
            process: ArcSwapOption::from(None),
            tx: connection_tx,
            broker_tx,
            input_to: ArcSwapOption::from(None),
            snapshot: ArcSwap::default(),
            dead: AtomicBool::new(false),
            logged_in: AtomicBool::new(false),
        }
    }

    /// The body the loop dispatches to; `None` between bindings.
    pub fn body(&self) -> Option<Arc<Process>> {
        self.process.load_full()
    }

    /// Point the loop at `body` — the binding module's write, nobody else's.
    pub(crate) fn set_body(&self, body: Option<Arc<Process>>) {
        self.process.store(body);
    }

    /// Queue `op` for the connection task; `Err` once that task has exited.
    pub fn send(&self, op: ConnectionOp) -> Result<(), SendError<ConnectionOp>> {
        self.tx.send(op)
    }

    /// A sender for effects recorded against this connection.
    pub(crate) fn sender(&self) -> UnboundedSender<ConnectionOp> {
        self.tx.clone()
    }

    /// An `input_to` is waiting for the next line.
    pub fn awaits_input(&self) -> bool {
        self.input_to.load().is_some()
    }

    /// The waiting `input_to`, without taking it — the GC marker's peek.
    pub(crate) fn input_to(&self) -> Option<Arc<InputTo>> {
        self.input_to.load_full()
    }

    /// Set, or clear, the function the next line goes to.
    pub(crate) fn set_input_to(&self, input_to: Option<InputTo>) {
        self.input_to.store(input_to.map(Arc::new));
    }

    /// Take the waiting `input_to`, leaving none.
    pub(crate) fn take_input_to(&self) -> Option<Arc<InputTo>> {
        self.input_to.swap(None)
    }

    /// `detach` has run on this connection.
    pub fn is_dead(&self) -> bool {
        self.dead.load(Ordering::Acquire)
    }

    /// Record that `detach` ran.
    pub(crate) fn mark_dead(&self) {
        self.dead.store(true, Ordering::Release);
    }

    /// `logon()` returned non-zero on this connection.
    pub fn is_logged_in(&self) -> bool {
        self.logged_in.load(Ordering::Acquire)
    }

    /// Record a successful `logon()`.
    pub(crate) fn set_logged_in(&self) {
        self.logged_in.store(true, Ordering::Release);
    }

    /// What the session knows about this client right now.
    pub fn snapshot(&self) -> Arc<Snapshot> {
        self.snapshot.load_full()
    }

    /// Mirror `session`; a store only when something changed.
    pub(crate) fn refresh(&self, session: &Session) {
        let next = Snapshot::of(session);
        if **self.snapshot.load() != next {
            self.snapshot.store(Arc::new(next));
        }
    }
}

impl PartialEq for Connection {
    fn eq(&self, other: &Self) -> bool {
        self.address == other.address
    }
}

impl Eq for Connection {}

#[cfg(test)]
mod tests {
    use lpc_rs_telnet::Session;

    use super::*;

    const IAC: u8 = 255;
    const SB: u8 = 250;
    const SE: u8 = 240;
    const DO: u8 = 253;
    const NAWS: u8 = 31;
    const GMCP: u8 = 201;

    #[test]
    fn a_fresh_session_knows_nothing() {
        assert_eq!(Snapshot::of(&Session::new()), Snapshot::default());
    }

    #[test]
    fn the_snapshot_reads_naws_and_option_state() {
        let mut session = Session::new();
        session.feed(&[IAC, DO, GMCP, IAC, SB, NAWS, 0, 100, 0, 40, IAC, SE]);
        assert_eq!(
            Snapshot::of(&session),
            Snapshot {
                cols: 100,
                rows: 40,
                charset: None,
                gmcp: true,
                mxp: false,
                eor: false,
            }
        );
    }

    #[test]
    fn refresh_stores_only_on_change() {
        let (tx, _rx) = tokio::sync::mpsc::unbounded_channel();
        let (broker_tx, _broker_rx) = flume::unbounded();
        let addr = "127.0.0.1:1".parse().unwrap();
        let connection = Connection::new(addr, tx, broker_tx);
        let mut session = Session::new();
        let before = connection.snapshot();
        connection.refresh(&session);
        assert!(
            Arc::ptr_eq(&before, &connection.snapshot()),
            "unchanged: same Arc"
        );
        session.feed(&[IAC, DO, GMCP]);
        connection.refresh(&session);
        assert!(connection.snapshot().gmcp);
    }

    #[test]
    fn a_fresh_connection_is_unbound_and_flagless() {
        let (tx, _rx) = tokio::sync::mpsc::unbounded_channel();
        let connection = Connection::new(
            "127.0.0.1:1".parse().unwrap(),
            tx,
            flume::unbounded().0,
        );
        assert!(connection.body().is_none());
        assert!(!connection.awaits_input());
        assert!(!connection.is_dead());
        assert!(!connection.is_logged_in());
        connection.set_body(Some(Arc::new(Process::default())));
        assert!(connection.body().is_some());
        connection.mark_dead();
        connection.set_logged_in();
        assert!(connection.is_dead());
        assert!(connection.is_logged_in());
        assert!(connection.take_input_to().is_none());
    }
}
