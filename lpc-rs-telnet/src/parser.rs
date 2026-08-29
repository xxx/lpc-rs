//! The IAC state machine: bytes in, frames out. Holds no policy and never
//! errors — what it cannot make sense of it counts and steps past.

// Until session.rs lands (B5); it removes this.
#![cfg_attr(not(test), expect(dead_code))]

use crate::opt::{DO, DONT, IAC, SB, SE, WILL, WONT};

/// One decoded unit of the wire.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Frame {
    /// A data byte, `IAC IAC` already collapsed.
    Byte(u8),
    /// A bare command: GA, NOP, EOR, AYT, …
    Command(u8),
    /// DO / DONT / WILL / WONT and its option.
    Negotiate(u8, u8),
    /// A complete subnegotiation: option and payload, `IAC IAC` collapsed.
    Sub(u8, Vec<u8>),
}

#[derive(Debug)]
enum Mode {
    Data,
    Iac,
    Negotiate(u8),
    SubOption,
    Sub {
        opt: u8,
        payload: Vec<u8>,
    },
    SubIac {
        opt: u8,
        payload: Vec<u8>,
    },
    /// An over-long subnegotiation: discarding until `IAC SE`.
    SubSkip,
    SubSkipIac,
}

#[derive(Debug)]
pub(crate) struct Parser {
    mode: Mode,
    max_sub: usize,
    /// Sequences that made no sense and were stepped past.
    pub(crate) malformed: u64,
}

impl Parser {
    /// A parser whose subnegotiation payloads hold at most `max_sub` bytes.
    pub(crate) fn new(max_sub: usize) -> Self {
        Self {
            mode: Mode::Data,
            max_sub,
            malformed: 0,
        }
    }

    pub(crate) fn feed(&mut self, bytes: &[u8], frames: &mut Vec<Frame>) {
        for &byte in bytes {
            self.push(byte, frames);
        }
    }

    fn push(&mut self, byte: u8, frames: &mut Vec<Frame>) {
        let mode = std::mem::replace(&mut self.mode, Mode::Data);
        let next = match mode {
            Mode::Data => match byte {
                IAC => Mode::Iac,
                data => {
                    frames.push(Frame::Byte(data));
                    Mode::Data
                }
            },
            Mode::Iac => match byte {
                IAC => {
                    frames.push(Frame::Byte(IAC));
                    Mode::Data
                }
                DO | DONT | WILL | WONT => Mode::Negotiate(byte),
                SB => Mode::SubOption,
                SE => {
                    self.malformed += 1;
                    Mode::Data
                }
                239..=249 => {
                    frames.push(Frame::Command(byte));
                    Mode::Data
                }
                _ => {
                    self.malformed += 1;
                    Mode::Data
                }
            },
            Mode::Negotiate(command) => {
                frames.push(Frame::Negotiate(command, byte));
                Mode::Data
            }
            Mode::SubOption => Mode::Sub {
                opt: byte,
                payload: Vec::new(),
            },
            Mode::Sub { opt, mut payload } => match byte {
                IAC => Mode::SubIac { opt, payload },
                _ if payload.len() >= self.max_sub => {
                    self.malformed += 1;
                    Mode::SubSkip
                }
                data => {
                    payload.push(data);
                    Mode::Sub { opt, payload }
                }
            },
            Mode::SubIac { opt, mut payload } => match byte {
                IAC => {
                    payload.push(IAC);
                    Mode::Sub { opt, payload }
                }
                SE => {
                    frames.push(Frame::Sub(opt, payload));
                    Mode::Data
                }
                command => {
                    // A command where SE belonged: the subnegotiation is lost,
                    // the command is not.
                    self.malformed += 1;
                    self.mode = Mode::Iac;
                    self.push(command, frames);
                    return;
                }
            },
            Mode::SubSkip => match byte {
                IAC => Mode::SubSkipIac,
                _ => Mode::SubSkip,
            },
            Mode::SubSkipIac => match byte {
                SE => Mode::Data,
                _ => Mode::SubSkip,
            },
        };
        self.mode = next;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const GA: u8 = 249;

    fn parse(bytes: &[u8]) -> (Vec<Frame>, u64) {
        let mut parser = Parser::new(16);
        let mut frames = Vec::new();
        parser.feed(bytes, &mut frames);
        (frames, parser.malformed)
    }

    #[test]
    fn iac_iac_is_a_data_byte() {
        let (frames, malformed) = parse(&[b'a', IAC, IAC, b'b']);
        assert_eq!(
            frames,
            [Frame::Byte(b'a'), Frame::Byte(IAC), Frame::Byte(b'b')]
        );
        assert_eq!(malformed, 0);
    }

    #[test]
    fn negotiation_is_three_bytes() {
        let (frames, _) = parse(&[IAC, DO, 201, b'x']);
        assert_eq!(frames, [Frame::Negotiate(DO, 201), Frame::Byte(b'x')]);
    }

    #[test]
    fn a_bare_command_is_two_bytes() {
        let (frames, _) = parse(&[IAC, GA]);
        assert_eq!(frames, [Frame::Command(GA)]);
    }

    #[test]
    fn a_subnegotiation_collapses_doubled_iac() {
        let (frames, malformed) = parse(&[IAC, SB, 31, 0, IAC, IAC, 0, 24, IAC, SE]);
        assert_eq!(frames, [Frame::Sub(31, vec![0, IAC, 0, 24])]);
        assert_eq!(malformed, 0);
    }

    #[test]
    fn a_bare_se_is_malformed_and_skipped() {
        let (frames, malformed) = parse(&[IAC, SE, b'x']);
        assert_eq!(frames, [Frame::Byte(b'x')]);
        assert_eq!(malformed, 1);
    }

    #[test]
    fn an_unknown_iac_command_is_skipped() {
        let (frames, malformed) = parse(&[IAC, 100, b'x']);
        assert_eq!(frames, [Frame::Byte(b'x')]);
        assert_eq!(malformed, 1);
    }

    #[test]
    fn an_iac_at_the_end_waits_for_the_next_feed() {
        let mut parser = Parser::new(16);
        let mut frames = Vec::new();
        parser.feed(&[b'a', IAC], &mut frames);
        assert_eq!(frames, [Frame::Byte(b'a')]);
        parser.feed(&[DO, 1], &mut frames);
        assert_eq!(frames, [Frame::Byte(b'a'), Frame::Negotiate(DO, 1)]);
    }

    #[test]
    fn an_overlong_subnegotiation_is_dropped_up_to_its_se() {
        let mut bytes = vec![IAC, SB, 201];
        bytes.extend(std::iter::repeat_n(7u8, 20));
        bytes.extend([IAC, IAC, IAC, SE, b'x']);
        let (frames, malformed) = parse(&bytes);
        assert_eq!(frames, [Frame::Byte(b'x')]);
        assert_eq!(malformed, 1);
    }

    #[test]
    fn an_iac_command_inside_a_subnegotiation_drops_it_and_reads_the_command() {
        let (frames, malformed) = parse(&[IAC, SB, 201, b'a', IAC, DO, 1]);
        assert_eq!(frames, [Frame::Negotiate(DO, 1)]);
        assert_eq!(malformed, 1);
    }

    #[test]
    fn a_subnegotiation_split_across_feeds_completes() {
        let mut parser = Parser::new(16);
        let mut frames = Vec::new();
        parser.feed(&[IAC, SB, 201, b'a'], &mut frames);
        parser.feed(&[b'b', IAC], &mut frames);
        parser.feed(&[SE], &mut frames);
        assert_eq!(frames, [Frame::Sub(201, b"ab".to_vec())]);
    }
}
