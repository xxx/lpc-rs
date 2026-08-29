# lpc-rs-telnet

The telnet session for lpc-rs: one connection's protocol state, with no
socket. Bytes in, events out; ops in, bytes out. See the crate docs.

## Fuzzing

Two cargo-fuzz targets live in `fuzz/`: `feed` (client bytes) and `session`
(client bytes interleaved with ops). They need nightly and `cargo-fuzz`:

    cargo make fuzz-telnet                     # feed, 60 s
    TARGET=session SECONDS=300 cargo make fuzz-telnet

A crash lands in `fuzz/artifacts/<target>/`; minimise it with
`cargo +nightly fuzz tmin <target> <file>` and turn it into a table test.
