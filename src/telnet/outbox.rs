use std::sync::Arc;

use bytes::{Buf, BytesMut};
use lpc_rs_telnet::{Op, Session};
use tracing::info;

use crate::telnet::connection::Connection;

/// The one line a client gets in place of what it could not take.
pub(crate) const TRUNCATED: &str = "*** Output truncated ***\n";

/// One client's pending bytes and their bound: past the high mark text to
/// it is dropped, until what is pending falls below the low one.
#[derive(Debug)]
pub(crate) struct Outbox {
    /// Drained from the session, not yet taken by the socket.
    out: BytesMut,
    /// Pending bytes at which truncation starts; `0` for no bound.
    high: usize,
    /// Pending bytes below which a truncated client's output resumes.
    low: usize,
    /// Past the high-water mark and not yet under the low one.
    overflowed: bool,
    /// Ops dropped since the mark was crossed.
    dropped: usize,
    /// Whose bytes; carries the flag readers outside the loop see.
    connection: Arc<Connection>,
}

impl Outbox {
    /// An empty outbox for `connection`, bounded at `max_pending_output`
    /// pending bytes; `0` for no bound.
    pub(crate) fn new(connection: Arc<Connection>, max_pending_output: usize) -> Self {
        Self {
            out: BytesMut::with_capacity(4096),
            high: max_pending_output,
            // A low mark of 0 could never be gone below; a tiny bound would
            // truncate forever.
            low: (max_pending_output / 2).max(1),
            overflowed: false,
            dropped: 0,
            connection,
        }
    }

    /// Queue `op` on `session`, or count it dropped while overflowed.
    pub(crate) fn send(&mut self, session: &mut Session, op: Op<'_>) {
        if self.overflowed {
            self.dropped += 1;
        } else {
            session.send(op);
        }
    }

    /// Move what `session` queued in; crossing the high-water mark appends
    /// [`TRUNCATED`] and starts dropping.
    pub(crate) fn fill_from(&mut self, session: &mut Session) {
        session.drain_output(&mut self.out);
        if !self.overflowed && self.high != 0 && self.out.len() >= self.high {
            session.send(Op::Text(TRUNCATED));
            session.drain_output(&mut self.out);
            self.overflowed = true;
            self.connection.set_overflowed(true);
            info!(
                "{} is not taking its output: {} bytes pending; dropping text until it does",
                self.connection.address,
                self.out.len()
            );
        }
    }

    /// What the socket has not taken yet.
    pub(crate) fn pending(&self) -> &[u8] {
        &self.out
    }

    /// Text to the client is being dropped.
    pub(crate) fn is_overflowed(&self) -> bool {
        self.overflowed
    }

    /// `n` more bytes reached the socket.
    pub(crate) fn wrote(&mut self, n: usize) {
        self.out.advance(n);
        if self.overflowed && self.out.len() < self.low {
            self.overflowed = false;
            self.connection.set_overflowed(false);
            info!(
                "{} is taking its output again; {} messages were dropped",
                self.connection.address, self.dropped
            );
            self.dropped = 0;
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_telnet::Session;
    use tokio::sync::mpsc;

    use super::*;

    fn outbox(max_pending_output: usize) -> (Outbox, Arc<Connection>) {
        let (tx, _rx) = mpsc::unbounded_channel();
        let connection = Arc::new(Connection::new("127.0.0.1:1".parse().unwrap(), tx));
        (
            Outbox::new(connection.clone(), max_pending_output),
            connection,
        )
    }

    /// 64 lines of 1023 x's plus CR LF: 65,600 bytes, past the high-water mark.
    fn flood(outbox: &mut Outbox, session: &mut Session) {
        let line = format!("{}\n", "x".repeat(1023));
        for _ in 0..64 {
            outbox.send(session, Op::Text(&line));
        }
        outbox.fill_from(session);
    }

    #[test]
    fn under_the_mark_nothing_is_dropped() {
        let (mut outbox, connection) = outbox(64 * 1024);
        let mut session = Session::new();
        // Drain initial negotiation.
        let mut buf = BytesMut::with_capacity(4096);
        session.drain_output(&mut buf);
        buf.clear();
        outbox.send(&mut session, Op::Text("hi\n"));
        outbox.fill_from(&mut session);
        assert_eq!(outbox.pending(), b"hi\r\n");
        assert!(!outbox.is_overflowed());
        assert!(!connection.is_overflowed());
        outbox.wrote(4);
        assert!(outbox.pending().is_empty());
    }

    #[test]
    fn crossing_the_mark_appends_the_marker_once_and_drops_what_follows() {
        let (mut outbox, connection) = outbox(64 * 1024);
        let mut session = Session::new();
        // Drain initial negotiation.
        let mut buf = BytesMut::with_capacity(4096);
        session.drain_output(&mut buf);
        buf.clear();
        flood(&mut outbox, &mut session);
        assert!(outbox.is_overflowed());
        assert!(connection.is_overflowed());
        assert!(outbox.pending().ends_with(b"*** Output truncated ***\r\n"));
        let len = outbox.pending().len();
        outbox.send(&mut session, Op::Text("late\n"));
        outbox.send(&mut session, Op::Prompt("> "));
        outbox.fill_from(&mut session);
        assert_eq!(outbox.pending().len(), len, "nothing more is queued");
        assert_eq!(outbox.dropped, 2);
    }

    #[test]
    fn output_resumes_below_the_low_water_mark() {
        let (mut outbox, connection) = outbox(64 * 1024);
        let mut session = Session::new();
        // Drain initial negotiation.
        let mut buf = BytesMut::with_capacity(4096);
        session.drain_output(&mut buf);
        buf.clear();
        flood(&mut outbox, &mut session);
        let len = outbox.pending().len();
        outbox.wrote(len - 32 * 1024);
        assert!(outbox.is_overflowed(), "at the low mark, still overflowed");
        outbox.wrote(1);
        assert!(!outbox.is_overflowed());
        assert!(!connection.is_overflowed());
        assert_eq!(outbox.dropped, 0);
        outbox.send(&mut session, Op::Text("late\n"));
        outbox.fill_from(&mut session);
        assert!(outbox.pending().ends_with(b"late\r\n"));
    }

    #[test]
    fn a_bound_of_zero_never_truncates() {
        let (mut outbox, connection) = outbox(0);
        let mut session = Session::new();
        // Drain initial negotiation.
        let mut buf = BytesMut::with_capacity(4096);
        session.drain_output(&mut buf);
        buf.clear();
        flood(&mut outbox, &mut session);
        assert!(!outbox.is_overflowed());
        assert!(!connection.is_overflowed());
        assert_eq!(outbox.pending().len(), 65_600, "every byte kept");
    }

    #[test]
    fn a_tiny_bound_still_resumes_once_drained() {
        let (mut outbox, connection) = outbox(1);
        let mut session = Session::new();
        // Drain initial negotiation.
        let mut buf = BytesMut::with_capacity(4096);
        session.drain_output(&mut buf);
        buf.clear();
        outbox.send(&mut session, Op::Text("hi\n"));
        outbox.fill_from(&mut session);
        assert!(outbox.is_overflowed());
        let len = outbox.pending().len();
        outbox.wrote(len);
        assert!(!outbox.is_overflowed());
        assert!(!connection.is_overflowed());
    }
}
