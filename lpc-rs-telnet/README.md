# lpc-rs-telnet

The telnet session for lpc-rs: one connection's protocol state, with no
socket. Bytes in, events out; ops in, bytes out. See the crate docs.

## Fuzzing

Two cargo-fuzz targets live in `fuzz/`: `feed` compares whole and fragmented
client input in both input modes, and `session` interleaves input with output,
negotiation, protocol-event reads and terminal reports. They need nightly and
`cargo-fuzz` (`cargo install cargo-fuzz --version 0.13.2 --locked`). From the
repository root:

    python3 fuzz/run.py --project telnet --seconds 60
    python3 fuzz/run.py --project telnet --target feed --seconds 60 --coverage

Committed `fuzz/seeds/` inputs cover negotiation, Unicode and payloads larger
than the 8 KiB limits. Generated inputs stay in ignored `fuzz/corpus/` directories.
The session input consists of records with a one-byte operation and a little-endian
two-byte payload length; operation tags follow the decoder in
`fuzz/fuzz_targets/session.rs`.

A crash lands in `fuzz/artifacts/<target>/`; minimise it with
`cargo +nightly fuzz tmin --fuzz-dir lpc-rs-telnet/fuzz <target> <file>`
from the repository root and turn it into a table test. See the shared
[fuzzing guide](../fuzz/README.md) for CI budgets, coverage and harness boundaries.
If you keep a local `Makefile.toml` (it is git-ignored), `cargo make fuzz-telnet`
can launch individual targets with `TARGET` and `SECONDS` variables; use the
shared runner to include the curated seeds and the larger input limit.
