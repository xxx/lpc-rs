#![no_main]

use bytes::BytesMut;
use libfuzzer_sys::fuzz_target;
use lpc_rs_telnet::{Event, InputMode, Opt, Session, Stats, TerminalInfo};

#[derive(Debug, PartialEq, Eq)]
struct Observation {
    events: Vec<Event>,
    output: BytesMut,
    stats: Stats,
    terminal: TerminalInfo,
    naws: Option<(u16, u16)>,
    charset: Option<String>,
    options: Vec<bool>,
}

fn observe(data: &[u8], chunk_size: usize, mode: InputMode) -> Observation {
    let mut session = Session::new();
    session.set_input_mode(mode);
    let mut events = Vec::new();
    let mut output = BytesMut::new();
    for chunk in data.chunks(chunk_size) {
        session.feed(chunk);
        while let Some(event) = session.next_event() {
            if let Event::Character(text) = &event {
                assert_eq!(text.chars().count(), 1);
            }
            events.push(event);
        }
        session.drain_output(&mut output);
    }
    session.drain_output(&mut output);
    assert!(session.next_protocol_event().is_none());
    let mut drained = BytesMut::new();
    session.drain_output(&mut drained);
    assert!(drained.is_empty());
    Observation {
        events,
        output,
        stats: session.stats(),
        terminal: session.terminal().clone(),
        naws: session.naws(),
        charset: session.charset().map(str::to_owned),
        options: (0..=255)
            .map(|byte| session.is_on(Opt::from(byte)))
            .collect(),
    }
}

fuzz_target!(|data: &[u8]| {
    let chunk_size = data.first().map_or(7, |byte| usize::from(*byte) % 31 + 1);
    for mode in [InputMode::Line, InputMode::Character] {
        assert_eq!(
            observe(data, data.len().max(1), mode),
            observe(data, chunk_size, mode),
            "fragmentation changed the session in {mode:?} mode"
        );
    }
});
