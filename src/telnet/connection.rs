use std::{net::SocketAddr, sync::Arc};

use arc_swap::{ArcSwap, ArcSwapAny, ArcSwapOption};
use flume::Sender as FlumeSender;
use lpc_rs_telnet::{Opt, Session};
use tokio::sync::mpsc::{Sender, error::SendError};

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

/// A connection from a user
#[derive(Debug)]
pub struct Connection {
    /// The address of the client.
    pub address: SocketAddr,

    /// The process that this connection is attached to.
    /// This is basically the player's in-game body object.
    pub process: ArcSwapAny<Option<Arc<Process>>>,

    /// The channel we use to send messages to the socket connection's thread.
    pub tx: Sender<ConnectionOp>,

    /// The channel we use to send messages to the [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker).
    pub broker_tx: FlumeSender<BrokerOp>,

    /// The function to call when we receive input.
    pub input_to: ArcSwapOption<InputTo>,

    /// The loop's mirror of the session, for readers that never see it.
    snapshot: ArcSwap<Snapshot>,
}

impl Connection {
    /// Creates a new [`Connection`].
    pub fn new(
        address: SocketAddr,
        connection_tx: Sender<ConnectionOp>,
        broker_tx: FlumeSender<BrokerOp>,
    ) -> Self {
        Self {
            address,
            process: ArcSwapAny::from(None),
            tx: connection_tx,
            broker_tx,
            input_to: ArcSwapOption::from(None),
            snapshot: ArcSwap::default(),
        }
    }

    /// What the session knows about this client right now.
    pub fn snapshot(&self) -> Arc<Snapshot> {
        self.snapshot.load_full()
    }

    /// Queue `op` for the connection task; `Err` once that task has exited.
    pub async fn send(&self, op: ConnectionOp) -> Result<(), SendError<ConnectionOp>> {
        self.tx.send(op).await
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
        let (tx, _rx) = tokio::sync::mpsc::channel(1);
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
}
