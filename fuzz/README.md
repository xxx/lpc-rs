# Fuzz testing

The root fuzz workspace tests the compiler and runtime; the Telnet crate keeps
its own workspace at `lpc-rs-telnet/fuzz`. Both use cargo-fuzz and AddressSanitizer.

| Project | Target | Contract |
|---|---|---|
| core | `compiler` | Compile arbitrary source in both null modes and render its diagnostics; errors are allowed, internal compiler bugs and invalid source spans are failures. |
| core | `save_format` | Parse arbitrary saved values and save-file lines; successfully serialized values have stable canonical output. |
| core | `save_round_trip` | Generate bounded scalar, bytes, array and mapping values; compare restored contents, including float bits, in both null modes. |
| core | `grammar` | Compare all captures with a naive matcher for small generated patterns; exercise native, parser-package and DGD grammar text with bounded parse work. |
| core | `formatting` | Exercise format parsing, bounded rendering, numeric scan cursors, by-reference writes and `%s`/`%d` formatting and scanning identities. |
| core | `stm` | Compare generated starts, reads, writes, removals, commits and aborts with a reference model; check GC refuses while snapshots are live. |
| telnet | `feed` | Compare events, wire replies, counters and negotiated state for whole and fragmented input in line and character modes. |
| telnet | `session` | Interleave input, output, protocol-event reads, terminal reports, negotiation and oversized payloads with extensions initially enabled and disabled. |

## Running campaigns

Install nightly with `clippy`, `rust-src`, `rustfmt` and `llvm-tools-preview`, then install
`cargo-fuzz` (`cargo install cargo-fuzz --version 0.13.2 --locked`). From the
repository root:

```sh
python3 fuzz/run.py --project core --seconds 60
python3 fuzz/run.py --project telnet --seconds 60
python3 fuzz/run.py --project core --target compiler --seconds 300 --coverage
```

The runner checks the standalone lockfile, formatting and warning-free Clippy, builds every target
in that project, and gives each selected target its own time allowance. Inputs
may grow to 32 KiB, enough to cross Telnet's 8 KiB limits. Cargo output goes to
the shared `target/fuzz/` directory. Logs, generated corpora, crash artifacts and
coverage stay in the corresponding fuzz workspace and are gitignored.

Every fuzz command explicitly selects the host from `rustc +nightly -vV`,
including when the installed cargo-fuzz binary was built for another target.

Each target reads both its accumulated `corpus/<target>/` and its committed
`seeds/<target>/`. New inputs go only into `corpus/`; keep curated seeds small
and named for the behavior they exercise. The large Telnet seeds intentionally
cross line and subnegotiation limits, including escaped IAC payloads.

`--coverage` replays both input directories after the campaign and writes
`coverage/<target>/report.txt` and `coverage.profdata`. The report measures
source lines and regions reached by that target, not semantic correctness or
ordinary unit-test coverage.

## Harness boundaries

The compiler uses an in-memory `SourceReader` and scoped diagnostic storage;
generated source cannot load host files. An optional `\n// fuzz-source\n` divider
separates the contents of `fuzz.h` from the root source. `/parent.c` is a fixed
small inherited program. Runtime initialization is not executed by this target.

Grammar seeds use `\n// fuzz-input\n` to separate grammar text from input text.
The differential subset has at most five pattern elements and six input words;
frontend parses have explicit work, depth and result limits.

Generated saved-value trees have at most eight container levels and 64 generated
nodes, with scalar mapping keys. Object identity, class definitions and cyclic
graphs require separate generators. The text parser still receives malformed
and over-depth inputs directly.

The formatting harness renders only fixed widths and precisions at most 256,
with bounded scalar arguments; it parses larger specifications without rendering
them. Scan calls use real by-reference cells and check conversion counts, scalar
assignments and unchanged cells on format errors. Resource-limit probes and richer argument trees can
extend this target separately.

The STM model uses four cells and four concurrent attempts, with at most 256
operations per input. Each operation is four bytes: kind, attempt, cell, value.
Kinds modulo seven are start, read, write, remove, commit, abort and GC. Reads
of an attempt's own writes do not create snapshot dependencies; blind writes
commit in order. The GC operation's value byte selects root cells, and quiescent
passes must reclaim exactly the unrooted cells. Object publication, merges,
effect delivery and payload-graph collection remain covered by their ordinary
tests rather than this initial model.

The root crate exposes internal harnesses only with the `fuzzing` feature.
Cargo-fuzz's `cfg(fuzzing)` selects the system allocator so ASan can observe
allocations; normal builds and tests retain their existing allocator.

## CI and regressions

The required `CI` workflow calls the fuzz workflow and requires both project
jobs to pass. Pull requests and pushes run 20 seconds per target. Daily and
manual campaigns run 300 seconds per target and generate coverage reports.
Builds and accumulated corpora have separate caches per project, so a nightly
toolchain update preserves the corpus; only `master` saves caches. Each run
uploads corpora, merged coverage, reports, logs and crash artifacts for
14 days, including on failure.

Minimize a failure with the target's own workspace, for example:

```sh
CARGO_TARGET_DIR="$PWD/target/fuzz" cargo +nightly fuzz tmin \
  --fuzz-dir fuzz compiler fuzz/artifacts/compiler/<artifact>
```

Turn confirmed failures into inline behavior tests beside the affected code,
and retain a small input under the relevant `seeds/` directory. Run the workspace
tests as well as the reproducing target before considering the regression fixed.
