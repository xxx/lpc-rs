# AGENTS.md

Guidance for AI agents (and humans) working in this repository.

## What this is

An [LPC](https://mud.fandom.com/wiki/LPC) compiler and gamedriver for MUDs, written in
Rust. It is a work-in-progress personal project: the compiler, preprocessor, interpreter,
efun library, and the transactional runtime are the live areas of development.

## Workspace layout

A Cargo workspace. The root crate `lpc-rs` (in `src/`) is the compiler + interpreter +
gamedriver library; the subcrates:

| Crate | Purpose |
|---|---|
| `lpc-rs-lpcc` | CLI: compile and run a single LPC file (`cargo run -p lpc-rs-lpcc <file>`) |
| `lpc-rs-asm` | Instruction set definition |
| `lpc-rs-core` | Core types: `LpcPath`, `LpcType`, registers, function metadata |
| `lpc-rs-function-support` | Function prototypes, signatures, `call_other` support |
| `lpc-rs-errors` | Error types and the `lpc_error!` / `lpc_bug!` macros |
| `lpc-rs-utils` | `Config` (env-var driven) and misc helpers |
| `lpc-rs-driver` | The gamedriver server crate (sparse, `main.rs` only) |

Key areas of `src/`:

- `compiler/` — lexer, parser (lalrpop), preprocessor, semantic analysis, codegen
- `interpreter/vm/` — `Vm`, `GlobalState`, GC pass, object loading
- `interpreter/stm/` — the transactional runtime: committer, attempt runner, GC pass
- `interpreter/efun/` — one module per efun, with inline `#[cfg(test)]` modules
- `interpreter/task/`, `task_context.rs`, `call_frame.rs` — the execution context
- `util/process_builder.rs` — the shared object-creation compile core (see below)
- `tests/` — integration tests (`compiler_test`, `concurrency_test`, `parser_test`) with
  fixtures in `tests/fixtures/`
- `doc/glossary.md` — project terminology; `doc/apply/` — LPC apply-hook docs

## Build and test

```sh
cargo build --workspace
cargo test --workspace            # lib + integration + doctests, every crate
cargo clippy --workspace --all-targets   # must be warning-free
cargo fmt --check     # fmt config uses nightly-only options; on stable the
                      # "unstable features" warnings are expected and harmless
```

Always pass `--workspace` (or at least build each member). The root `lpc-rs`
package alone does not compile the sibling crates: `cargo build`, `cargo test`,
and `cargo clippy` without `--workspace` from the root target only `lpc-rs`, so
a change to a public API (e.g. removing a trait the `lpc-rs-lpcc` CLI imports)
can look green and break the workspace.

CI (`.github/workflows/ci.yml`) runs: `cargo clippy --workspace`,
`cargo nextest run --workspace --no-fail-fast`, doctests, and
`RUSTDOCFLAGS="-D warnings" cargo doc --workspace` — doc comments must be warning-free
(broken intra-doc links fail CI).

`.cargo/config` sets `--cfg tokio_unstable` for all builds. Runtime configuration is
entirely environment variables (`.env` supported); see `default.env` for the commented
list. Compile-time constants live in `src/compile_time_config.rs`.

Quick start: create `lib/hello.c` with `void create() { dump("hi"); }` and run
`cargo run -p lpc-rs-lpcc lib/hello.c` (`-p` selects the CLI and builds it plus
`lpc-rs`; it is not the whole-workspace gate above).

## Architecture facts that constrain changes

- **The STM committer is the sole serialization control.** There is no GIL, no
  write-write adjudication, and no global lock. Transactions (attempts) re-run on
  conflict via the attempt runner in `interpreter/stm/retry.rs`.
- **Object creation has two explicit placements.** Compile is one shared core
  (`compile_to_process` in `util/process_builder.rs`, reached through
  `compile_process_from_path` / `compile_process_from_code`). Placement is either
  *physical* (`ObjectSpace::insert_process_physical`, blind, no cell — bootstrap and
  test fixtures only, reached through `ObjectSpace::create_process_from_path` /
  `create_process_from_code` and the `Vm` forwarders of the same name) or *transactional*
  (`TaskContext::insert_process_transactional` / `txn_insert_process`, a cell write +
  deferred physical insert flushed at commit). In-game contexts (`EfunContext`,
  `TaskContext`) deliberately have **no** physical create path; if a call site wants
  to physically insert in an in-game context, that is a design smell.
- **Clones do not compile.** The production clone path is
  `ObjectSpace::create_clone_process` (mint clone ID, no insert) + transactional
  insert; the `clone_object` efun exercises it.
- **GC is one atomic committer message** (quiescence check, upvalue-cell cull, world
  sweep). It refuses rather than blocks when a transaction is in flight.
- **`apply_insert` / `apply_remove`** in the stm effects module are commit-time
  physical-map effect application; do not confuse them with the placement fns above.

## Conventions

- **No phase/step tokens** ("D1", "C5", "step 2", "candidate") in filenames, code,
  comments, or commit messages. Handoff/plan docs under `local/` may carry them;
  `local/` is gitignored by design and holds personal notes, handoffs, and scratch.
- **Comments carry one fact the reader cannot get from the code, in one sentence.**
  Exceptions: hazard comments (name the wrong edit), external facts, and public API
  docs. Multi-sentence argument comments get cut to the fact.
- Commit messages: plain descriptive subject, no tokens. The repo history is kept
  token-free; amend/rebase only unpushed commits and verify `git log` against
  `origin/master` first.
- `clippy.toml` disallows `std::sync::mpsc` channels and `std::sync::RwLock` /
  `tokio::sync::RwLock`; use `flume` and `parking_lot::RwLock` instead.
- Tests live inline as `#[cfg(test)]` modules next to the code (efun, stm, vm, task),
  with cross-cutting scenarios in `tests/`. Test-only helpers should be
  `#[cfg(test)]`-gated rather than left as dead code.
- `Makefile.toml` (cargo-make coverage tasks) and `deny.toml` (cargo-deny) are
  local-only, gitignored tooling, not part of the shared repo.
