#![no_main]

use bytes::BytesMut;
use libfuzzer_sys::fuzz_target;
use lpc_rs_telnet::{Event, InputMode, MAX_LINE, Op, Opt, Session};

/// The fuzzer's own owned shape of an `Op`, plus client bytes.
#[derive(Debug)]
enum Operation {
    Feed(Vec<u8>),
    CharacterInput,
    LineInput,
    Next,
    Text(String),
    Mxp(String),
    Prompt(String),
    EchoOff,
    EchoOn,
    Gmcp { package: String, payload: String },
    Mssp(Vec<(String, Vec<String>)>),
    NextProtocol,
    Negotiate { option: u8, enable: bool },
    Terminal(String),
    Oversized { subnegotiation: bool },
}

fn operations(mut data: &[u8]) -> impl Iterator<Item = Operation> + '_ {
    std::iter::from_fn(move || {
        if data.len() < 3 {
            return None;
        }
        let tag = data[0] % 15;
        let length = usize::from(u16::from_le_bytes([data[1], data[2]])).min(data.len() - 3);
        let payload = &data[3..3 + length];
        data = &data[3 + length..];
        let text = || String::from_utf8_lossy(payload).into_owned();
        Some(match tag {
            0 => Operation::Feed(payload.to_vec()),
            1 => Operation::CharacterInput,
            2 => Operation::LineInput,
            3 => Operation::Next,
            4 => Operation::Text(text()),
            5 => Operation::Mxp(text()),
            6 => Operation::Prompt(text()),
            7 => Operation::EchoOff,
            8 => Operation::EchoOn,
            9 => {
                let text = text();
                let (package, payload) = text.split_once(' ').unwrap_or((&text, ""));
                Operation::Gmcp {
                    package: package.to_owned(),
                    payload: payload.to_owned(),
                }
            }
            10 => Operation::Mssp(if payload.is_empty() {
                Vec::new()
            } else {
                text()
                    .split('\x01')
                    .map(|variable| {
                        let mut fields = variable.split('\0');
                        let name = fields.next().unwrap().to_owned();
                        (name, fields.map(str::to_owned).collect())
                    })
                    .collect()
            }),
            11 => Operation::NextProtocol,
            12 => Operation::Negotiate {
                option: payload.first().copied().unwrap_or(201),
                enable: payload.get(1).is_none_or(|byte| byte % 2 == 0),
            },
            13 => Operation::Terminal(text()),
            _ => Operation::Oversized {
                subnegotiation: payload.first().is_some_and(|byte| byte % 2 != 0),
            },
        })
    })
}

fn subnegotiation(session: &mut Session, option: u8, payload: &[u8]) {
    let mut wire = vec![255, 250, option];
    for &byte in payload {
        wire.push(byte);
        if byte == 255 {
            wire.push(byte);
        }
    }
    wire.extend([255, 240]);
    session.feed(&wire);
}

fn check_event(event: &Event) {
    if let Event::Character(text) = event {
        assert_eq!(text.chars().count(), 1);
    }
}

fuzz_target!(|data: &[u8]| {
    let operations: Vec<_> = operations(data).take(128).collect();
    for enabled in [false, true] {
        let mut session = Session::new();
        let mut out = BytesMut::new();
        if enabled {
            session.feed(&[
                255, 253, 201, 255, 253, 70, 255, 253, 91, 255, 253, 25, 255, 251, 24,
            ]);
            assert!(session.is_on(Opt::Gmcp));
            assert!(session.is_on(Opt::Mssp));
        }
        session.drain_output(&mut out);
        out.clear();
        for operation in &operations {
            match operation {
                Operation::Feed(bytes) => session.feed(bytes),
                Operation::CharacterInput => session.set_input_mode(InputMode::Character),
                Operation::LineInput => session.set_input_mode(InputMode::Line),
                Operation::Next => {
                    if let Some(event) = session.next_event() {
                        check_event(&event);
                    }
                }
                Operation::NextProtocol => {
                    if let Some(event) = session.next_protocol_event() {
                        assert!(!matches!(event, Event::Line(_) | Event::Character(_)));
                    }
                    let _ = session.has_pending_input();
                }
                Operation::Negotiate { option, enable } => {
                    session.feed(&[255, if *enable { 253 } else { 254 }, *option]);
                }
                Operation::Terminal(name) => {
                    let payload = [b"\0".as_slice(), name.as_bytes()].concat();
                    subnegotiation(&mut session, 24, &payload);
                }
                Operation::Oversized {
                    subnegotiation: sub,
                } => {
                    let payload = vec![b'a'; MAX_LINE + 1];
                    if *sub {
                        subnegotiation(&mut session, 201, &payload);
                    } else {
                        session.feed(&payload);
                        session.feed(b"\r\n");
                    }
                }
                Operation::Text(s) => session.send(Op::Text(s)),
                Operation::Mxp(s) => session.send(Op::Mxp(s)),
                Operation::Prompt(s) => session.send(Op::Prompt(s)),
                Operation::EchoOff => session.send(Op::EchoOff),
                Operation::EchoOn => session.send(Op::EchoOn),
                Operation::Gmcp { package, payload } => session.send(Op::Gmcp { package, payload }),
                Operation::Mssp(vars) => {
                    let values: Vec<Vec<&str>> = vars
                        .iter()
                        .map(|(_, v)| v.iter().map(String::as_str).collect())
                        .collect();
                    let pairs: Vec<(&str, &[&str])> = vars
                        .iter()
                        .zip(&values)
                        .map(|((name, _), v)| (name.as_str(), v.as_slice()))
                        .collect();
                    session.send(Op::Mssp(&pairs));
                }
            }
            session.drain_output(&mut out);
            out.clear();
        }
        while let Some(event) = session.next_event() {
            check_event(&event);
        }
        session.drain_output(&mut out);
        out.clear();
        session.drain_output(&mut out);
        assert!(out.is_empty());
        assert!(session.next_protocol_event().is_none());
    }
});
