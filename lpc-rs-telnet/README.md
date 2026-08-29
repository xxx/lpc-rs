# lpc-rs-telnet

The telnet session for lpc-rs: one connection's protocol state, with no
socket. Bytes in, events out; ops in, bytes out. See the crate docs.

## Fuzzing

Two cargo-fuzz targets live in `fuzz/`: `feed` (client bytes) and `session`
(client bytes interleaved with ops). They need nightly and `cargo-fuzz`
(`cargo install cargo-fuzz`):

    cd lpc-rs-telnet
    cargo +nightly fuzz run feed -- -max_total_time=60
    cargo +nightly fuzz run session -- -max_total_time=60

A crash lands in `fuzz/artifacts/<target>/`; minimise it with
`cargo +nightly fuzz tmin <target> <file>` and turn it into a table test.
If you keep a local `Makefile.toml` (it is git-ignored), `cargo make fuzz-telnet`
wraps the same command with `TARGET` and `SECONDS` variables.
