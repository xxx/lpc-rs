#![no_main]

use arbitrary::Arbitrary;
use bytes::BytesMut;
use libfuzzer_sys::fuzz_target;
use lpc_rs_telnet::{Op, Session};

/// The fuzzer's own owned shape of an `Op`, plus client bytes.
#[derive(Arbitrary, Debug)]
enum Step {
    Feed(Vec<u8>),
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
        while session.next_event().is_some() {}
        session.drain_output(&mut out);
        out.clear();
    }
    session.drain_output(&mut out);
    assert!(out.is_empty());
});
