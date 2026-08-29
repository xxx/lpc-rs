#![no_main]

use bytes::BytesMut;
use libfuzzer_sys::fuzz_target;
use lpc_rs_telnet::Session;

// Feed in odd-sized chunks so IAC sequences straddle feeds.
fuzz_target!(|data: &[u8]| {
    let mut session = Session::new();
    let mut out = BytesMut::new();
    // Connect offers are already queued; drain them before asserting quiescence.
    session.drain_output(&mut out);
    out.clear();
    for chunk in data.chunks(7) {
        session.feed(chunk);
        while session.next_event().is_some() {}
        session.drain_output(&mut out);
        out.clear();
    }
    session.drain_output(&mut out);
    assert!(out.is_empty());
    assert!(session.next_event().is_none());
});
