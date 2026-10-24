//! The session: negotiation policy, line assembly, and the MUD extensions.

use std::collections::VecDeque;

use bytes::BytesMut;

use crate::{
    opt::{DO, DONT, EOR_CMD, GA, Opt, WILL, WONT},
    parser::{Frame, Parser},
    table::{Reply, Table},
    terminal::{Terminal, TerminalInfo},
    wire,
};

/// Bytes an inbound line may hold; the rest of a longer line is dropped.
pub const MAX_LINE: usize = 8192;

/// How incoming text is divided into application events.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum InputMode {
    /// Wait for a line terminator.
    #[default]
    Line,
    /// Deliver one Unicode scalar or control character.
    Character,
}

/// What the client did, as far as the caller needs to know.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Event {
    /// One line of input, terminator removed, decoded as lossy UTF-8.
    Line(String),
    /// One Unicode scalar or control character, decoded as lossy UTF-8.
    Character(String),
    /// The client's window size, in characters.
    Naws {
        /// Columns.
        cols: u16,
        /// Rows.
        rows: u16,
    },
    /// The charset both sides agreed on.
    Charset(String),
    /// A GMCP message: the package name and whatever followed it, raw.
    Gmcp {
        /// `Core.Hello`, `Char.Vitals`, …
        package: String,
        /// The rest of the message, usually JSON; empty when there was none.
        payload: String,
    },
    /// The client enabled MSSP; answer with [`Op::Mssp`].
    MsspRequested,
    /// Input lost bytes past [`MAX_LINE`] while buffering a line.
    LineTruncated,
}

/// What the caller asks the session to send.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op<'a> {
    /// Literal text: LF becomes CR LF.
    Text(&'a str),
    /// MXP markup, framed as secure lines (ESC[1z). Dropped when MXP is off.
    Mxp(&'a str),
    /// Text followed by the prompt mark the client negotiated: EOR, else GA
    /// unless SGA is on, else nothing.
    Prompt(&'a str),
    /// Ask the client to stop echoing (we send `WILL ECHO`).
    EchoOff,
    /// Let the client echo again (`WONT ECHO`).
    EchoOn,
    /// A GMCP message. Dropped when GMCP is off.
    Gmcp {
        /// The package name.
        package: &'a str,
        /// The message body; empty sends the name alone.
        payload: &'a str,
    },
    /// MSSP variables, each with its values. Dropped when MSSP is off.
    Mssp(&'a [(&'a str, &'a [&'a str])]),
}

/// Counters the caller may report.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Stats {
    /// Wire sequences that made no sense and were stepped past.
    pub malformed: u64,
    /// Lines that lost bytes past [`MAX_LINE`].
    pub truncated: u64,
    /// Ops dropped because their option was off.
    pub dropped: u64,
}

/// What we will do for an option.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Local {
    /// Never.
    Refuse,
    /// If the client asks.
    Accept,
    /// Asked at connect; also if the client asks.
    Offer,
    /// Only when we ask; a client asking first is refused.
    OnDemand,
}

/// What we let the client do.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Remote {
    /// Never.
    Refuse,
    /// If the client offers.
    Accept,
    /// Asked at connect; also if the client offers.
    Request,
}

type Policy = fn(Opt) -> (Local, Remote);

/// Which side may enable each option.
fn default_policy(opt: Opt) -> (Local, Remote) {
    match opt {
        Opt::Echo => (Local::OnDemand, Remote::Refuse),
        Opt::Sga => (Local::Accept, Remote::Accept),
        Opt::Charset => (Local::Offer, Remote::Accept),
        Opt::Naws => (Local::Refuse, Remote::Request),
        Opt::Eor => (Local::Offer, Remote::Accept),
        Opt::Gmcp => (Local::Offer, Remote::Refuse),
        Opt::Mxp => (Local::Offer, Remote::Refuse),
        Opt::Mssp => (Local::Offer, Remote::Refuse),
        Opt::Ttype => (Local::Refuse, Remote::Request),
        Opt::Mccp2 | Opt::Other(_) => (Local::Refuse, Remote::Refuse),
    }
}

/// The offers made at connect, in wire order.
const OFFERS: [Opt; 7] = [
    Opt::Naws,
    Opt::Charset,
    Opt::Gmcp,
    Opt::Mxp,
    Opt::Eor,
    Opt::Mssp,
    Opt::Ttype,
];

// MXP line modes: [1z opens one secure line; [7z locks the client's
// default to locked, so plain text is never parsed as markup.
const MXP_SECURE_LINE: &[u8] = b"\x1b[1z";
const MXP_LOCK_LOCKED: &[u8] = b"\x1b[7z";

const CHARSET_REQUEST: u8 = 1;
const CHARSET_ACCEPTED: u8 = 2;
const CHARSET_REJECTED: u8 = 3;
const CHARSET_TTABLE_IS: u8 = 4;
const CHARSET_TTABLE_REJECTED: u8 = 6;
const MSSP_VAR: u8 = 1;
const MSSP_VAL: u8 = 2;
const CR: u8 = b'\r';
const LF: u8 = b'\n';
const NUL: u8 = 0;

#[derive(Debug)]
enum Incoming {
    Data(VecDeque<u8>),
    Event(Event),
}

/// One connection's telnet state. See the crate docs.
#[derive(Debug)]
pub struct Session {
    policy: Policy,
    table: Table,
    parser: Parser,
    frames: Vec<Frame>,
    line: Vec<u8>,
    line_overflowed: bool,
    input_mode: InputMode,
    restore_sga: bool,
    after_cr: bool,
    naws: Option<(u16, u16)>,
    charset: Option<String>,
    terminal: Terminal,
    incoming: VecDeque<Incoming>,
    out: BytesMut,
    stats: Stats,
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

impl Session {
    /// A session for a client that just connected; its offers are already queued.
    pub fn new() -> Self {
        Self::with_policy(default_policy)
    }

    fn with_policy(policy: Policy) -> Self {
        let mut session = Self {
            policy,
            table: Table::default(),
            parser: Parser::new(MAX_LINE),
            frames: Vec::new(),
            line: Vec::new(),
            line_overflowed: false,
            input_mode: InputMode::Line,
            restore_sga: false,
            after_cr: false,
            naws: None,
            charset: None,
            terminal: Terminal::default(),
            incoming: VecDeque::new(),
            out: BytesMut::new(),
            stats: Stats::default(),
        };
        for opt in OFFERS {
            let reply = match policy(opt) {
                (Local::Offer, _) => session.table.ask_will(opt.into()),
                (_, Remote::Request) => session.table.ask_do(opt.into()),
                _ => None,
            };
            session.reply(reply);
        }
        session
    }

    /// Queue client data and answer negotiations; drain events and replies after.
    pub fn feed(&mut self, bytes: &[u8]) {
        let mut frames = std::mem::take(&mut self.frames);
        self.parser.feed(bytes, &mut frames);
        for frame in frames.drain(..) {
            match frame {
                Frame::Byte(byte) => match self.incoming.back_mut() {
                    Some(Incoming::Data(bytes)) => bytes.push_back(byte),
                    _ => self
                        .incoming
                        .push_back(Incoming::Data(VecDeque::from([byte]))),
                },
                Frame::Command(_) => {}
                Frame::Negotiate(command, opt) => self.negotiate(command, opt),
                Frame::Sub(opt, payload) => self.subnegotiation(Opt::from(opt), &payload),
            }
        }
        // An idle connection shouldn't keep a large read's high-water capacity.
        frames.shrink_to(256);
        self.frames = frames;
    }

    /// Something to send. Bytes queue up in the output buffer.
    pub fn send(&mut self, op: Op<'_>) {
        match op {
            Op::Text(s) => wire::text(&mut self.out, s),
            Op::Mxp(s) => {
                if self.is_on(Opt::Mxp) {
                    for line in s.split_inclusive('\n') {
                        self.out.extend_from_slice(MXP_SECURE_LINE);
                        wire::text(&mut self.out, line);
                    }
                    // A line the markup left open would parse what follows.
                    if !s.ends_with('\n') {
                        self.out.extend_from_slice(MXP_LOCK_LOCKED);
                    }
                } else {
                    self.stats.dropped += 1;
                }
            }
            Op::Prompt(s) => {
                wire::text(&mut self.out, s);
                if self.is_on(Opt::Eor) {
                    wire::command(&mut self.out, EOR_CMD);
                } else if !self.is_on(Opt::Sga) {
                    wire::command(&mut self.out, GA);
                }
            }
            Op::EchoOff => {
                let reply = self.table.ask_will(Opt::Echo.into());
                self.reply(reply);
            }
            Op::EchoOn => {
                let reply = self.table.ask_wont(Opt::Echo.into());
                self.reply(reply);
            }
            Op::Gmcp { package, payload } => {
                if self.is_on(Opt::Gmcp) {
                    let mut bytes = package.as_bytes().to_vec();
                    if !payload.is_empty() {
                        bytes.push(b' ');
                        bytes.extend_from_slice(payload.as_bytes());
                    }
                    wire::subnegotiation(&mut self.out, Opt::Gmcp.into(), &bytes);
                } else {
                    self.stats.dropped += 1;
                }
            }
            Op::Mssp(vars) => {
                if self.is_on(Opt::Mssp) {
                    let mut bytes = Vec::new();
                    for (name, values) in vars {
                        bytes.push(MSSP_VAR);
                        bytes.extend_from_slice(name.as_bytes());
                        for value in *values {
                            bytes.push(MSSP_VAL);
                            bytes.extend_from_slice(value.as_bytes());
                        }
                    }
                    wire::subnegotiation(&mut self.out, Opt::Mssp.into(), &bytes);
                } else {
                    self.stats.dropped += 1;
                }
            }
        }
    }

    /// Select how unread text is delivered, preserving any partial input.
    /// Character mode requests SGA; line mode withdraws it if it was off on entry.
    pub fn set_input_mode(&mut self, mode: InputMode) {
        if self.input_mode == mode {
            return;
        }
        self.input_mode = mode;
        match mode {
            InputMode::Character => {
                if !self.line.is_empty() {
                    self.incoming
                        .push_front(Incoming::Data(std::mem::take(&mut self.line).into()));
                }
                if std::mem::take(&mut self.line_overflowed) {
                    self.incoming
                        .push_front(Incoming::Event(Event::LineTruncated));
                }
                self.restore_sga = self.table.us_on(Opt::Sga.into());
                let reply = self.table.ask_will(Opt::Sga.into());
                self.reply(reply);
            }
            InputMode::Line => {
                if !self.restore_sga {
                    let reply = self.table.ask_wont(Opt::Sga.into());
                    self.reply(reply);
                }
            }
        }
    }

    /// Decode the next event in the current mode, leaving later data unread.
    pub fn next_event(&mut self) -> Option<Event> {
        while let Some(incoming) = self.incoming.pop_front() {
            match incoming {
                Incoming::Event(event) => return Some(event),
                Incoming::Data(mut bytes) => {
                    let byte = bytes.pop_front().expect("queued data is nonempty");
                    if !bytes.is_empty() {
                        self.incoming.push_front(Incoming::Data(bytes));
                    }
                    if let Some(event) = self.byte(byte) {
                        return Some(event);
                    }
                }
            }
        }
        None
    }

    /// Move everything queued for the client into `into`.
    pub fn drain_output(&mut self, into: &mut BytesMut) {
        into.extend_from_slice(&self.out);
        self.out.clear();
    }

    /// Whether the option is in effect: the client's side for NAWS and TTYPE, ours
    /// otherwise — `is_on(Sga)` means we suppress go-ahead, since RFC 858 is
    /// about our own transmissions.
    pub fn is_on(&self, opt: Opt) -> bool {
        let raw = u8::from(opt);
        match opt {
            Opt::Naws | Opt::Ttype => self.table.him_on(raw),
            _ => self.table.us_on(raw),
        }
    }

    /// The client's window size, if it said.
    pub fn naws(&self) -> Option<(u16, u16)> {
        self.naws
    }

    /// The charset agreed on, if any; output is UTF-8 regardless.
    pub fn charset(&self) -> Option<&str> {
        self.charset.as_deref()
    }

    /// The client's TTYPE/MTTS reports, if negotiated.
    pub fn terminal(&self) -> &TerminalInfo {
        &self.terminal.info
    }

    /// Counters so far.
    pub fn stats(&self) -> Stats {
        Stats {
            malformed: self.parser.malformed,
            ..self.stats
        }
    }

    fn reply(&mut self, reply: Option<Reply>) {
        if let Some(Reply(command, opt)) = reply {
            wire::negotiate(&mut self.out, command, opt);
        }
    }

    fn byte(&mut self, byte: u8) -> Option<Event> {
        if self.after_cr && matches!(byte, LF | NUL) {
            self.after_cr = false;
            return None;
        }
        self.after_cr = byte == CR;
        if self.input_mode == InputMode::Character {
            self.line.push(byte);
            return match std::str::from_utf8(&self.line) {
                Ok(text) => {
                    let event = Event::Character(text.to_owned());
                    self.line.clear();
                    Some(event)
                }
                Err(error) => {
                    let invalid = error.error_len()?;
                    if invalid < self.line.len() {
                        self.incoming
                            .push_front(Incoming::Data(self.line.split_off(invalid).into()));
                    }
                    self.line.clear();
                    Some(Event::Character("\u{fffd}".into()))
                }
            };
        }
        match byte {
            CR | LF => return Some(self.end_line()),
            NUL => {}
            data => {
                if self.line.len() < MAX_LINE {
                    self.line.push(data);
                } else if !self.line_overflowed {
                    self.line_overflowed = true;
                    self.stats.truncated += 1;
                }
            }
        }
        None
    }

    fn end_line(&mut self) -> Event {
        let text = String::from_utf8_lossy(&self.line).into_owned();
        self.line.clear();
        if std::mem::take(&mut self.line_overflowed) {
            self.incoming
                .push_front(Incoming::Event(Event::LineTruncated));
        }
        Event::Line(text)
    }

    fn negotiate(&mut self, command: u8, raw: u8) {
        let opt = Opt::from(raw);
        let (local, remote) = (self.policy)(opt);
        let was_on = self.table.us_on(raw);
        let was_remote_on = self.table.him_on(raw);
        let reply = match command {
            DO => self
                .table
                .recv_do(raw, matches!(local, Local::Accept | Local::Offer)),
            DONT => self.table.recv_dont(raw),
            WILL => self
                .table
                .recv_will(raw, matches!(remote, Remote::Accept | Remote::Request)),
            WONT => self.table.recv_wont(raw),
            _ => None,
        };
        self.reply(reply);
        if !was_on && self.table.us_on(raw) {
            self.enabled_us(opt);
        }
        if opt == Opt::Ttype && was_remote_on != self.table.him_on(raw) {
            if self.table.him_on(raw) {
                self.terminal.start();
                wire::subnegotiation(&mut self.out, raw, &[1]);
            } else {
                self.terminal = Terminal::default();
            }
        }
    }

    /// What we send once the client agrees to something we do.
    fn enabled_us(&mut self, opt: Opt) {
        match opt {
            Opt::Charset => {
                let mut payload = vec![CHARSET_REQUEST, b' '];
                payload.extend_from_slice(b"UTF-8");
                wire::subnegotiation(&mut self.out, opt.into(), &payload);
            }
            Opt::Mssp => self
                .incoming
                .push_back(Incoming::Event(Event::MsspRequested)),
            Opt::Mxp => {
                wire::subnegotiation(&mut self.out, opt.into(), &[]);
                self.out.extend_from_slice(MXP_LOCK_LOCKED);
            }
            _ => {}
        }
    }

    fn subnegotiation(&mut self, opt: Opt, payload: &[u8]) {
        match opt {
            Opt::Ttype => {
                if self.is_on(opt) && self.terminal.receive(payload) {
                    wire::subnegotiation(&mut self.out, opt.into(), &[1]);
                }
            }
            Opt::Naws => {
                if let [c1, c2, r1, r2] = *payload {
                    let size = (u16::from_be_bytes([c1, c2]), u16::from_be_bytes([r1, r2]));
                    self.naws = Some(size);
                    self.incoming.push_back(Incoming::Event(Event::Naws {
                        cols: size.0,
                        rows: size.1,
                    }));
                }
            }
            Opt::Charset => self.charset_subnegotiation(payload),
            Opt::Gmcp => {
                let text = String::from_utf8_lossy(payload);
                let (package, payload) = match text.split_once(' ') {
                    Some((package, rest)) => (package.to_owned(), rest.trim_start().to_owned()),
                    None => (text.trim().to_owned(), String::new()),
                };
                self.incoming
                    .push_back(Incoming::Event(Event::Gmcp { package, payload }));
            }
            _ => {}
        }
    }

    fn charset_subnegotiation(&mut self, payload: &[u8]) {
        let Some((&sub, rest)) = payload.split_first() else {
            return;
        };
        match sub {
            CHARSET_REQUEST => {
                let rest = strip_ttable(rest);
                let Some((&separator, names)) = rest.split_first() else {
                    wire::subnegotiation(&mut self.out, Opt::Charset.into(), &[CHARSET_REJECTED]);
                    return;
                };
                let utf8 = names.split(|&b| b == separator).any(|name| {
                    name.eq_ignore_ascii_case(b"UTF-8") || name.eq_ignore_ascii_case(b"UTF8")
                });
                if utf8 {
                    let mut reply = vec![CHARSET_ACCEPTED];
                    reply.extend_from_slice(b"UTF-8");
                    wire::subnegotiation(&mut self.out, Opt::Charset.into(), &reply);
                    self.agree_charset("UTF-8");
                } else {
                    wire::subnegotiation(&mut self.out, Opt::Charset.into(), &[CHARSET_REJECTED]);
                }
            }
            CHARSET_ACCEPTED => {
                let name = String::from_utf8_lossy(rest).into_owned();
                self.agree_charset(&name);
            }
            CHARSET_TTABLE_IS => {
                wire::subnegotiation(
                    &mut self.out,
                    Opt::Charset.into(),
                    &[CHARSET_TTABLE_REJECTED],
                );
            }
            _ => {}
        }
    }

    fn agree_charset(&mut self, name: &str) {
        self.charset = Some(name.to_owned());
        self.incoming
            .push_back(Incoming::Event(Event::Charset(name.to_owned())));
    }
}

/// RFC 2066 lets a REQUEST open with `[TTABLE ]<version>`; we don't do
/// translation tables, so skip both.
fn strip_ttable(rest: &[u8]) -> &[u8] {
    if rest.starts_with(b"[TTABLE")
        && let Some(end) = rest.iter().position(|&b| b == b']')
    {
        return rest.get(end + 2..).unwrap_or(&[]);
    }
    rest
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opt::{IAC, SB, SE};

    const ECHO: u8 = 1;
    const SGA: u8 = 3;
    const EOR: u8 = 25;
    const NAWS: u8 = 31;
    const CHARSET: u8 = 42;
    const MSSP: u8 = 70;
    const MCCP2: u8 = 86;
    const MXP: u8 = 91;
    const GMCP: u8 = 201;

    /// A session past its offers, so `out` starts empty.
    fn connected() -> Session {
        let mut session = Session::new();
        out(&mut session);
        session
    }

    fn out(session: &mut Session) -> Vec<u8> {
        let mut buf = BytesMut::new();
        session.drain_output(&mut buf);
        buf.to_vec()
    }

    fn events(session: &mut Session) -> Vec<Event> {
        std::iter::from_fn(|| session.next_event()).collect()
    }

    #[test]
    fn connect_offers_extensions_in_order() {
        let mut session = Session::new();
        assert_eq!(
            out(&mut session),
            [
                IAC, DO, NAWS, IAC, WILL, CHARSET, IAC, WILL, GMCP, IAC, WILL, MXP, IAC, WILL, EOR,
                IAC, WILL, MSSP, IAC, DO, 24
            ]
        );
    }

    #[test]
    fn a_do_for_an_offered_option_turns_it_on_silently() {
        let mut s = connected();
        s.feed(&[IAC, DO, GMCP]);
        assert!(out(&mut s).is_empty());
        assert!(s.is_on(Opt::Gmcp));
    }

    #[test]
    fn a_dont_withdraws_an_enabled_local_option() {
        let mut s = connected();
        s.feed(&[IAC, DO, GMCP]);
        assert!(s.is_on(Opt::Gmcp));
        s.feed(&[IAC, DONT, GMCP]);
        assert!(!s.is_on(Opt::Gmcp));
        assert_eq!(out(&mut s), [IAC, WONT, GMCP]);
        s.send(Op::Gmcp {
            package: "Core.Ping",
            payload: "",
        });
        assert!(out(&mut s).is_empty());
        assert_eq!(s.stats().dropped, 1);
    }

    #[test]
    fn plain_reads_do_not_queue_a_chunk_per_byte() {
        let mut s = connected();
        s.feed(&[b'a'; 1024]);
        s.feed(&[b'b'; 1024]);
        assert!(
            s.incoming.len() <= 2,
            "two reads queued {} data chunks",
            s.incoming.len()
        );
        s.feed(b"\n");
        assert_eq!(
            events(&mut s),
            [Event::Line(format!(
                "{}{}",
                "a".repeat(1024),
                "b".repeat(1024)
            ))]
        );
    }

    #[test]
    fn ttype_is_negotiated_once_and_fragmented_reports_request_the_next_name() {
        let mut s = connected();
        s.feed(b"\xff\xfa\x18\0MTTS 269\xff\xf0");
        assert_eq!(s.terminal().colour_depth, None);
        s.feed(&[IAC, WILL, 24]);
        assert_eq!(out(&mut s), [IAC, SB, 24, 1, IAC, SE]);
        s.feed(&[IAC, WILL, 24]);
        assert!(out(&mut s).is_empty());
        for byte in b"\xff\xfa\x18\0CLIENT\xff\xf0" {
            s.feed(&[*byte]);
        }
        assert_eq!(out(&mut s), [IAC, SB, 24, 1, IAC, SE]);
        s.feed(b"\xff\xfa\x18\0XTERM-256COLOR\xff\xf0");
        assert_eq!(out(&mut s), [IAC, SB, 24, 1, IAC, SE]);
        s.feed(b"\xff\xfa\x18\0MTTS 269\xff\xf0");
        assert!(out(&mut s).is_empty());
        assert_eq!(
            s.terminal().colour_depth,
            Some(crate::ColourDepth::TrueColour)
        );
        assert_eq!(s.terminal().mtts, Some(269));
        s.feed(b"\xff\xfa\x18\0MTTS 0\xff\xf0");
        assert_eq!(s.terminal().mtts, Some(269));
    }

    #[test]
    fn disabling_ttype_clears_capabilities_and_a_new_exchange_starts_fresh() {
        let mut s = connected();
        s.feed(&[IAC, WILL, 24]);
        out(&mut s);
        s.feed(b"\xff\xfa\x18\0MTTS 269\xff\xf0");
        s.feed(&[IAC, WONT, 24]);
        assert!(!s.is_on(Opt::Ttype));
        assert_eq!(s.terminal(), &TerminalInfo::default());
        out(&mut s);
        s.feed(&[IAC, WILL, 24]);
        assert_eq!(out(&mut s), [IAC, DO, 24, IAC, SB, 24, 1, IAC, SE]);
        s.feed(b"\xff\xfa\x18\0DUMB\xff\xf0");
        assert_eq!(s.terminal().colour_depth, Some(crate::ColourDepth::Plain));
    }

    #[test]
    fn ttype_refusal_and_malformed_reports_do_not_generate_request_loops() {
        let mut s = connected();
        s.feed(&[IAC, WONT, 24]);
        assert!(out(&mut s).is_empty());
        s.feed(&[IAC, WILL, 24]);
        out(&mut s);
        s.feed(b"\xff\xfa\x18\x01ANSI\xff\xf0");
        s.feed(b"\xff\xfa\x18\0ANSI\n\xff\xf0");
        assert!(out(&mut s).is_empty());
        assert_eq!(s.terminal(), &TerminalInfo::default());
        s.feed(b"\xff\xfa\x18\0ANSI\xff\xf0");
        assert_eq!(out(&mut s), [IAC, SB, 24, 1, IAC, SE]);
        s.feed(b"\xff\xfa\x18\0ANSI\xff\xf0");
        assert!(out(&mut s).is_empty());
    }

    #[test]
    fn an_unsolicited_do_for_an_accepted_option_answers_will() {
        let mut s = connected();
        s.feed(&[IAC, DO, SGA]);
        assert_eq!(out(&mut s), [IAC, WILL, SGA]);
        assert!(s.is_on(Opt::Sga));
    }

    #[test]
    fn a_do_for_a_refused_option_answers_wont() {
        let mut s = connected();
        s.feed(&[IAC, DO, MCCP2, IAC, DO, 24]);
        assert_eq!(out(&mut s), [IAC, WONT, MCCP2, IAC, WONT, 24]);
    }

    #[test]
    fn a_stray_do_echo_is_refused() {
        let mut s = connected();
        s.feed(&[IAC, DO, ECHO]);
        assert_eq!(out(&mut s), [IAC, WONT, ECHO]);
        assert!(!s.is_on(Opt::Echo));
    }

    #[test]
    fn echo_off_and_on_around_input() {
        let mut s = connected();
        s.send(Op::EchoOff);
        assert_eq!(out(&mut s), [IAC, WILL, ECHO]);
        s.feed(&[IAC, DO, ECHO]);
        assert!(out(&mut s).is_empty());
        assert!(s.is_on(Opt::Echo));
        s.send(Op::EchoOn);
        assert_eq!(out(&mut s), [IAC, WONT, ECHO]);
        s.feed(&[IAC, DONT, ECHO]);
        assert!(out(&mut s).is_empty());
        assert!(!s.is_on(Opt::Echo));
    }

    #[test]
    fn an_unsolicited_will_for_a_refused_option_answers_dont() {
        let mut s = connected();
        s.feed(&[IAC, WILL, ECHO]);
        assert_eq!(out(&mut s), [IAC, DONT, ECHO]);
    }

    #[test]
    fn his_will_naws_completes_our_request() {
        let mut s = connected();
        s.feed(&[IAC, WILL, NAWS]);
        assert!(out(&mut s).is_empty());
        assert!(s.is_on(Opt::Naws));
    }

    #[test]
    fn every_terminator_ends_a_line() {
        let mut s = connected();
        s.feed(b"one\r\ntwo\r\0three\nfour\r");
        assert_eq!(
            events(&mut s),
            [
                Event::Line("one".into()),
                Event::Line("two".into()),
                Event::Line("three".into()),
                Event::Line("four".into()),
            ]
        );
    }

    #[test]
    fn a_line_waits_for_its_terminator() {
        let mut s = connected();
        s.feed(b"ab\r\ncd");
        assert_eq!(events(&mut s), [Event::Line("ab".into())]);
        s.feed(b"e\r");
        s.feed(b"\n");
        assert_eq!(events(&mut s), [Event::Line("cde".into())]);
    }

    #[test]
    fn nul_is_stripped_and_invalid_utf8_is_lossy() {
        let mut s = connected();
        s.feed(b"a\0b\xc3\r\n");
        assert_eq!(events(&mut s), [Event::Line("ab\u{FFFD}".into())]);
    }

    #[test]
    fn an_empty_line_is_a_line() {
        let mut s = connected();
        s.feed(b"\r\n");
        assert_eq!(events(&mut s), [Event::Line(String::new())]);
    }

    #[test]
    fn a_line_past_the_cap_is_truncated_and_reported() {
        let mut s = connected();
        let mut bytes = vec![b'a'; MAX_LINE + 1];
        bytes.extend(b"\r\n");
        s.feed(&bytes);
        let evs = events(&mut s);
        assert_eq!(evs.len(), 2);
        assert_eq!(evs[0], Event::Line("a".repeat(MAX_LINE)));
        assert_eq!(evs[1], Event::LineTruncated);
        assert_eq!(s.stats().truncated, 1);
    }

    #[test]
    fn malformed_wire_resyncs_and_counts() {
        let mut s = connected();
        let mut bytes = vec![IAC, SB, NAWS];
        bytes.extend(std::iter::repeat_n(0u8, MAX_LINE + 100));
        bytes.extend([IAC, SE]);
        bytes.extend(b"ok\r\n");
        s.feed(&bytes);
        assert_eq!(events(&mut s), [Event::Line("ok".into())]);
        assert_eq!(s.stats().malformed, 1);
    }

    #[test]
    fn a_doubled_iac_run_cannot_grow_a_subnegotiation_past_the_cap() {
        let mut s = connected();
        let mut bytes = vec![IAC, SB, GMCP];
        for _ in 0..(MAX_LINE + 100) {
            bytes.extend([IAC, IAC]);
        }
        bytes.extend([IAC, SE]);
        bytes.extend(b"ok\r\n");
        s.feed(&bytes);
        assert_eq!(events(&mut s), [Event::Line("ok".into())]);
        assert_eq!(s.stats().malformed, 1);
    }

    #[test]
    fn character_input_keeps_unicode_scalars_whole_at_every_split() {
        let text = "aé€🦀e\u{301}";
        for split in 0..=text.len() {
            let mut s = connected();
            s.set_input_mode(InputMode::Character);
            s.feed(&text.as_bytes()[..split]);
            let mut got = events(&mut s);
            s.feed(&text.as_bytes()[split..]);
            got.extend(events(&mut s));
            assert_eq!(
                got,
                text.chars()
                    .map(|c| Event::Character(c.to_string()))
                    .collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn character_input_replaces_invalid_utf8_without_losing_valid_following_text() {
        let bytes = b"\xc0\x80\xe2(\xa1\xe2\x82!\xf0\x80\x80\x80ok";
        for split in 0..=bytes.len() {
            let mut s = connected();
            s.set_input_mode(InputMode::Character);
            s.feed(&bytes[..split]);
            let mut got = events(&mut s);
            s.feed(&bytes[split..]);
            got.extend(events(&mut s));
            assert_eq!(
                got,
                String::from_utf8_lossy(bytes)
                    .chars()
                    .map(|c| Event::Character(c.to_string()))
                    .collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn character_input_preserves_control_keys_and_collapses_nvt_padding() {
        let mut s = connected();
        s.set_input_mode(InputMode::Character);
        let mut got = Vec::new();
        for byte in b"\x08\x7f\t\0\x1b[A\r\nZ\r\0Y\n" {
            s.feed(&[*byte]);
            got.extend(events(&mut s));
        }
        assert_eq!(
            got,
            "\u{8}\u{7f}\t\0\u{1b}[A\rZ\rY\n"
                .chars()
                .map(|c| Event::Character(c.to_string()))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn a_line_callback_can_select_characters_from_the_same_read() {
        let mut s = connected();
        s.feed("name\r\néx".as_bytes());
        assert_eq!(s.next_event(), Some(Event::Line("name".into())));
        s.set_input_mode(InputMode::Character);
        assert_eq!(
            events(&mut s),
            [Event::Character("é".into()), Event::Character("x".into())]
        );
    }

    #[test]
    fn a_character_callback_can_select_lines_from_the_same_read() {
        let mut s = connected();
        s.set_input_mode(InputMode::Character);
        s.feed(b"yrest\r\n");
        assert_eq!(s.next_event(), Some(Event::Character("y".into())));
        s.set_input_mode(InputMode::Line);
        assert_eq!(events(&mut s), [Event::Line("rest".into())]);
    }

    #[test]
    fn partial_lines_are_preserved_when_character_input_is_selected() {
        let mut s = connected();
        s.feed("éx".as_bytes());
        assert_eq!(s.next_event(), None);
        s.set_input_mode(InputMode::Character);
        assert_eq!(
            events(&mut s),
            [Event::Character("é".into()), Event::Character("x".into())]
        );
    }

    #[test]
    fn incomplete_characters_are_preserved_when_line_input_is_selected() {
        let mut s = connected();
        s.set_input_mode(InputMode::Character);
        s.feed(b"\xe2\x82");
        assert_eq!(s.next_event(), None);
        s.set_input_mode(InputMode::Line);
        s.feed(b"\xac next\r\n");
        assert_eq!(events(&mut s), [Event::Line("€ next".into())]);
    }

    #[test]
    fn nvt_padding_does_not_become_an_empty_line_after_a_character_callback() {
        let mut s = connected();
        s.set_input_mode(InputMode::Character);
        s.feed(b"\r\nnext\r\n");
        assert_eq!(s.next_event(), Some(Event::Character("\r".into())));
        s.set_input_mode(InputMode::Line);
        assert_eq!(events(&mut s), [Event::Line("next".into())]);
    }

    #[test]
    fn protocol_events_keep_their_position_between_input_callbacks() {
        let mut s = connected();
        s.set_input_mode(InputMode::Character);
        s.feed(b"a\xff\xfa\xc9Core.Ping\xff\xf0b\r\n");
        assert_eq!(s.next_event(), Some(Event::Character("a".into())));
        assert_eq!(
            s.next_event(),
            Some(Event::Gmcp {
                package: "Core.Ping".into(),
                payload: String::new()
            })
        );
        s.set_input_mode(InputMode::Line);
        assert_eq!(events(&mut s), [Event::Line("b".into())]);
    }

    #[test]
    fn character_mode_restores_sga_after_an_outstanding_acknowledgment() {
        let mut s = connected();
        s.set_input_mode(InputMode::Character);
        assert_eq!(out(&mut s), [IAC, WILL, SGA]);
        s.set_input_mode(InputMode::Character);
        assert!(out(&mut s).is_empty());
        s.set_input_mode(InputMode::Line);
        assert!(out(&mut s).is_empty());
        s.feed(&[IAC, DO, SGA]);
        assert_eq!(out(&mut s), [IAC, WONT, SGA]);
        s.feed(&[IAC, DONT, SGA]);
        assert!(!s.is_on(Opt::Sga));
    }

    #[test]
    fn character_mode_preserves_preexisting_sga() {
        let mut s = connected();
        s.feed(&[IAC, DO, SGA]);
        assert_eq!(out(&mut s), [IAC, WILL, SGA]);
        s.set_input_mode(InputMode::Character);
        s.set_input_mode(InputMode::Line);
        assert!(out(&mut s).is_empty());
        assert!(s.is_on(Opt::Sga));
    }

    #[test]
    fn refusing_sga_does_not_block_received_characters() {
        let mut s = connected();
        s.set_input_mode(InputMode::Character);
        out(&mut s);
        s.feed(&[IAC, DONT, SGA, b'x']);
        assert_eq!(events(&mut s), [Event::Character("x".into())]);
        s.set_input_mode(InputMode::Line);
        assert!(out(&mut s).is_empty());
    }

    #[test]
    fn text_gets_cr_lf() {
        let mut s = connected();
        s.send(Op::Text("hi\n"));
        assert_eq!(out(&mut s), b"hi\r\n");
    }

    #[test]
    fn output_stays_empty_after_a_drain() {
        let mut s = connected();
        s.send(Op::Text("x"));
        out(&mut s);
        assert!(out(&mut s).is_empty());
        assert_eq!(s.next_event(), None);
    }

    #[test]
    fn naws_is_stored_and_reported() {
        let mut s = connected();
        s.feed(&[IAC, WILL, NAWS, IAC, SB, NAWS, 0, 80, 0, 24, IAC, SE]);
        assert_eq!(events(&mut s), [Event::Naws { cols: 80, rows: 24 }]);
        assert_eq!(s.naws(), Some((80, 24)));
    }

    #[test]
    fn naws_with_a_doubled_iac_inside() {
        let mut s = connected();
        s.feed(&[IAC, SB, NAWS, 0, IAC, IAC, 0, 24, IAC, SE]);
        assert_eq!(
            events(&mut s),
            [Event::Naws {
                cols: 255,
                rows: 24
            }]
        );
    }

    #[test]
    fn a_naws_of_the_wrong_length_is_ignored() {
        let mut s = connected();
        s.feed(&[IAC, SB, NAWS, 0, 80, IAC, SE]);
        assert!(events(&mut s).is_empty());
        assert_eq!(s.naws(), None);
    }

    #[test]
    fn charset_do_sends_our_request() {
        let mut s = connected();
        s.feed(&[IAC, DO, CHARSET]);
        let mut expected = vec![IAC, SB, CHARSET, 1, b' '];
        expected.extend(b"UTF-8");
        expected.extend([IAC, SE]);
        assert_eq!(out(&mut s), expected);
    }

    #[test]
    fn charset_accepted_is_recorded() {
        let mut s = connected();
        s.feed(&[IAC, DO, CHARSET]);
        let mut bytes = vec![IAC, SB, CHARSET, 2];
        bytes.extend(b"UTF-8");
        bytes.extend([IAC, SE]);
        s.feed(&bytes);
        assert_eq!(events(&mut s), [Event::Charset("UTF-8".into())]);
        assert_eq!(s.charset(), Some("UTF-8"));
    }

    #[test]
    fn a_client_request_listing_utf8_is_accepted() {
        let mut s = connected();
        s.feed(&[IAC, WILL, CHARSET]);
        assert_eq!(out(&mut s), [IAC, DO, CHARSET]);
        let mut bytes = vec![IAC, SB, CHARSET, 1, b';'];
        bytes.extend(b"ISO-8859-1;utf-8");
        bytes.extend([IAC, SE]);
        s.feed(&bytes);
        let mut expected = vec![IAC, SB, CHARSET, 2];
        expected.extend(b"UTF-8");
        expected.extend([IAC, SE]);
        assert_eq!(out(&mut s), expected);
        assert_eq!(events(&mut s), [Event::Charset("UTF-8".into())]);
    }

    #[test]
    fn a_client_request_with_a_ttable_prefix_is_still_read() {
        let mut s = connected();
        let mut bytes = vec![IAC, SB, CHARSET, 1];
        bytes.extend(b"[TTABLE ]\x01 UTF-8");
        bytes.extend([IAC, SE]);
        s.feed(&bytes);
        assert_eq!(events(&mut s), [Event::Charset("UTF-8".into())]);
    }

    #[test]
    fn a_request_with_no_separator_is_rejected() {
        let mut s = connected();
        s.feed(&[IAC, SB, CHARSET, 1, IAC, SE]);
        assert_eq!(out(&mut s), [IAC, SB, CHARSET, 3, IAC, SE]);
        assert_eq!(s.charset(), None);
    }

    #[test]
    fn a_client_request_without_utf8_is_rejected() {
        let mut s = connected();
        let mut bytes = vec![IAC, SB, CHARSET, 1, b' '];
        bytes.extend(b"ISO-8859-1 CP437");
        bytes.extend([IAC, SE]);
        s.feed(&bytes);
        assert_eq!(out(&mut s), [IAC, SB, CHARSET, 3, IAC, SE]);
        assert_eq!(s.charset(), None);
    }

    #[test]
    fn charset_rejected_leaves_utf8_in_use() {
        let mut s = connected();
        s.feed(&[IAC, DO, CHARSET, IAC, SB, CHARSET, 3, IAC, SE]);
        out(&mut s);
        assert!(events(&mut s).is_empty());
        s.send(Op::Text("é"));
        assert_eq!(out(&mut s), "é".as_bytes());
    }

    #[test]
    fn charset_rejection_preserves_the_previous_agreement() {
        let mut s = connected();
        s.feed(&[IAC, DO, CHARSET]);
        out(&mut s);
        s.feed(b"\xff\xfa\x2a\x02UTF-8\xff\xf0");
        assert_eq!(events(&mut s), [Event::Charset("UTF-8".into())]);
        s.feed(&[IAC, SB, CHARSET, CHARSET_REJECTED, IAC, SE]);
        assert_eq!(s.charset(), Some("UTF-8"));
        assert!(events(&mut s).is_empty());
    }

    #[test]
    fn a_ttable_is_is_rejected() {
        let mut s = connected();
        s.feed(&[IAC, SB, CHARSET, 4, 1, IAC, SE]);
        assert_eq!(out(&mut s), [IAC, SB, CHARSET, 6, IAC, SE]);
    }

    #[test]
    fn gmcp_in_splits_package_from_payload() {
        let mut s = connected();
        s.feed(&[IAC, DO, GMCP]);
        let mut bytes = vec![IAC, SB, GMCP];
        bytes.extend(br#"Core.Hello {"client":"x"}"#);
        bytes.extend([IAC, SE, IAC, SB, GMCP]);
        bytes.extend(b"Core.Ping");
        bytes.extend([IAC, SE]);
        s.feed(&bytes);
        assert_eq!(
            events(&mut s),
            [
                Event::Gmcp {
                    package: "Core.Hello".into(),
                    payload: r#"{"client":"x"}"#.into()
                },
                Event::Gmcp {
                    package: "Core.Ping".into(),
                    payload: String::new()
                },
            ]
        );
    }

    #[test]
    fn gmcp_out_is_framed_when_on_and_dropped_when_off() {
        let mut s = connected();
        let op = Op::Gmcp {
            package: "Char.Vitals",
            payload: r#"{"hp":1}"#,
        };
        s.send(op);
        assert!(out(&mut s).is_empty());
        assert_eq!(s.stats().dropped, 1);
        s.feed(&[IAC, DO, GMCP]);
        s.send(op);
        let mut expected = vec![IAC, SB, GMCP];
        expected.extend(br#"Char.Vitals {"hp":1}"#);
        expected.extend([IAC, SE]);
        assert_eq!(out(&mut s), expected);
        s.send(Op::Gmcp {
            package: "Core.Ping",
            payload: "",
        });
        let mut expected = vec![IAC, SB, GMCP];
        expected.extend(b"Core.Ping");
        expected.extend([IAC, SE]);
        assert_eq!(out(&mut s), expected);
    }

    #[test]
    fn mxp_markup_is_framed_and_text_stays_literal() {
        let mut s = connected();
        s.send(Op::Text("a<b>&c"));
        assert_eq!(out(&mut s), b"a<b>&c");
        s.send(Op::Mxp("<send>x</send>"));
        assert!(out(&mut s).is_empty());
        assert_eq!(s.stats().dropped, 1);
        s.feed(&[IAC, DO, MXP]);
        let mut expected = vec![IAC, SB, MXP, IAC, SE];
        expected.extend_from_slice(b"\x1b[7z");
        assert_eq!(out(&mut s), expected);
        s.send(Op::Text("a<b>&c"));
        assert_eq!(out(&mut s), b"a<b>&c");
        s.send(Op::Mxp("<send>x</send>"));
        assert_eq!(out(&mut s), b"\x1b[1z<send>x</send>\x1b[7z");
        s.send(Op::Mxp("<b>two</b>\nlines\n"));
        assert_eq!(out(&mut s), b"\x1b[1z<b>two</b>\r\n\x1b[1zlines\r\n");
    }

    #[test]
    fn mxp_agreed_sends_the_mode_start() {
        let mut s = connected();
        s.feed(&[IAC, DO, MXP]);
        let mut expected = vec![IAC, SB, MXP, IAC, SE];
        expected.extend_from_slice(b"\x1b[7z");
        assert_eq!(out(&mut s), expected);
        assert!(s.is_on(Opt::Mxp));
    }

    #[test]
    fn prompt_ends_with_ga_then_nothing_then_eor() {
        let mut s = connected();
        s.send(Op::Prompt("> "));
        assert_eq!(out(&mut s), [b'>', b' ', IAC, 249]);
        s.feed(&[IAC, DO, SGA]);
        out(&mut s);
        s.send(Op::Prompt("> "));
        assert_eq!(out(&mut s), b"> ");
        s.feed(&[IAC, DO, EOR]);
        s.send(Op::Prompt("> "));
        assert_eq!(out(&mut s), [b'>', b' ', IAC, 239]);
    }

    #[test]
    fn mssp_is_offered_but_off_until_the_client_confirms() {
        let mut s = connected();
        s.send(Op::Mssp(&[("NAME", &["lpc-rs"])]));
        assert!(out(&mut s).is_empty());
        assert_eq!(s.stats().dropped, 1);
        assert!(!s.is_on(Opt::Mssp));
    }

    #[test]
    fn mssp_when_accepted_is_requested_and_framed() {
        let mut s = connected();
        s.feed(&[IAC, DO, MSSP]);
        assert!(out(&mut s).is_empty());
        assert_eq!(events(&mut s), [Event::MsspRequested]);
        s.send(Op::Mssp(&[("NAME", &["lpc-rs"]), ("PLAYERS", &["0"])]));
        let mut expected = vec![IAC, SB, MSSP, 1];
        expected.extend(b"NAME");
        expected.push(2);
        expected.extend(b"lpc-rs");
        expected.push(1);
        expected.extend(b"PLAYERS");
        expected.push(2);
        expected.extend(b"0");
        expected.extend([IAC, SE]);
        assert_eq!(out(&mut s), expected);
    }

    /// The property the wire demands, checked on every run: no input sequence
    /// panics, and nothing sticks in the queues.
    #[test]
    fn random_bytes_never_panic_and_always_drain() {
        let mut seed = 0x9E37_79B9_7F4A_7C15u64;
        let mut next = move || {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            seed
        };
        let mut s = Session::new();
        let mut buf = BytesMut::new();
        let mut fed = 0usize;
        while fed < 1 << 20 {
            let len = (next() % 64 + 1) as usize;
            let chunk: Vec<u8> = (0..len)
                .map(|_| match next() % 5 {
                    0 => IAC,
                    1 => 239 + (next() % 17) as u8,
                    _ => (next() & 0xFF) as u8,
                })
                .collect();
            s.set_input_mode(if next() % 2 == 0 {
                InputMode::Line
            } else {
                InputMode::Character
            });
            s.feed(&chunk);
            fed += len;
            if next() % 8 == 0 {
                s.send(Op::Text("random\n"));
                s.send(Op::EchoOff);
                s.send(Op::Prompt("> "));
                s.send(Op::Mxp("<b>x</b>"));
                s.send(Op::EchoOn);
            }
            while let Some(event) = s.next_event() {
                if let Event::Character(text) = event {
                    assert_eq!(text.chars().count(), 1);
                }
                s.set_input_mode(if next() % 2 == 0 {
                    InputMode::Line
                } else {
                    InputMode::Character
                });
            }
            s.drain_output(&mut buf);
            buf.clear();
        }
        s.drain_output(&mut buf);
        assert!(buf.is_empty());
        assert_eq!(s.next_event(), None);
    }
}
