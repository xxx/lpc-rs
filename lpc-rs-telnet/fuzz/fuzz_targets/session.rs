#![no_main]

use arbitrary::Arbitrary;
use bytes::BytesMut;
use libfuzzer_sys::fuzz_target;
use lpc_rs_telnet::{Event, InputMode, Op, Session};

/// The fuzzer's own owned shape of an `Op`, plus client bytes.
#[derive(Arbitrary, Debug)]
enum Step {
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
}

fuzz_target!(|steps: Vec<Step>| {
    let mut session = Session::new();
    let mut out = BytesMut::new();
    // Connect offers are already queued; drain them before asserting quiescence.
    session.drain_output(&mut out);
    out.clear();
    for step in &steps {
        match step {
            Step::Feed(bytes) => session.feed(bytes),
            Step::CharacterInput => session.set_input_mode(InputMode::Character),
            Step::LineInput => session.set_input_mode(InputMode::Line),
            Step::Next => {
                if let Some(Event::Character(text)) = session.next_event() {
                    assert_eq!(text.chars().count(), 1);
                }
            }
            Step::Text(s) => session.send(Op::Text(s)),
            Step::Mxp(s) => session.send(Op::Mxp(s)),
            Step::Prompt(s) => session.send(Op::Prompt(s)),
            Step::EchoOff => session.send(Op::EchoOff),
            Step::EchoOn => session.send(Op::EchoOn),
            Step::Gmcp { package, payload } => session.send(Op::Gmcp { package, payload }),
            Step::Mssp(vars) => {
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
        if let Event::Character(text) = event {
            assert_eq!(text.chars().count(), 1);
        }
    }
    session.drain_output(&mut out);
    assert!(out.is_empty());
});
