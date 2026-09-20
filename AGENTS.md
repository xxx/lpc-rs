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

CI (`.github/workflows/ci.yml`) runs formatting and warning-free Clippy across all
workspace targets, nextest and doctests, warning-free workspace docs (`--no-deps`),
and benchmark compilation in parallel jobs on stable Rust. Cargo checks use
`--locked`; broken intra-doc links fail CI. The final `ci` job is the required
branch-protection check and must fail if any preceding job fails or is skipped.

Rust dependency caches are separate per job and keyed by the toolchain, manifests,
lockfiles, and build settings; only `master` runs save them, and PRs restore them.
Dependabot proposes weekly updates for the commit-pinned GitHub Actions.

`.cargo/config` sets `--cfg tokio_unstable` for all builds. Runtime configuration is
entirely environment variables (`.env` requires driver `-e` or compiler `-c`); see
`default.env` for the commented list. Compile-time constants live in
`src/compile_time_config.rs`.

### Profiling the benches

`[profile.bench]` carries `debug = 1` and no `incremental` override, so sampled
stacks resolve and bench codegen matches release. Every bench installs a pprof
sampler on criterion's `--profile-time` hook (`benches/support/profiler.rs`):

```sh
cargo bench --bench bench_concurrency -- --profile-time 10 'm1_task_cost/fib10'
```

writes `target/criterion/<group>/<id>/profile/flamegraph.svg` — 999Hz, all
threads, no measurement in profile mode. External `perf` works on the bench
binaries too, now that symbols are present.

`cargo bench --features opcode-profile --bench bench_concurrency -- m1_task_cost`
adds per-workload opcode histograms (`[opcodes:fib10] ...`) to the output.

Quick start: create `lib/hello.c` with `void create() { dump("hi"); }` and run
`cargo run -p lpc-rs-lpcc lib/hello.c` (`-p` selects the CLI and builds it plus
`lpc-rs`; it is not the whole-workspace gate above).

## Keeping applies and ulib aligned

`ulib/` is shipped learning material, and part of the apply API's maintenance
surface. When adding, removing or changing an apply, update these in the same
change:

- The driver implementation and its behaviour tests, plus the apply constants
  in `src/interpreter/mod.rs` (or `CREATE_FUNCTION` in `lpc-rs-core`).
- Its canonical reference under `doc/apply/`, including arguments, result and
  omission semantics, execution timing, and caller/player context.
- Its live implementation or disabled example under `ulib/`, the adjacent
  source documentation, and `ulib/doc/applies.md`; adjust the learning guides
  and integration tests when their behaviour depends on the change.

This also applies when only semantics change: examples can still compile after
a default, permission, transaction or callback-context change. Do not treat
passing signature checks as proof that their explanations remain correct.

Run `cargo test --workspace --test ulib_test --test ulib`. The tests compare
documented signatures with the compiled examples and keep a reviewed digest of
the apply references, including the dynamic parser-handler contract. After
reviewing the affected ulib code and prose, refresh only the reviewed paths with
`python3 tests/update_ulib_apply_review.py doc/apply/<category>/<apply>.md`
and commit the review record with the changes; never refresh it just to silence
a failing test. See [the maintenance guide](doc/maintaining-ulib.md).

## Keeping driver comparisons accurate

When changing a feature covered by [the driver comparison](doc/driver-comparison.md),
review its overview cell and porting notes alongside the canonical reference.
Upstream claims need publicly accessible sources pinned to a reviewed revision;
record the review date and relevant build options. Distinguish source review
from cross-driver execution tests, and leave unchecked behaviour labelled as such.

## Architecture facts that constrain changes

- **The STM committer is the sole serialization control.** There is no GIL, no
  write-write adjudication, and no global lock. Transactions (attempts) re-run on
  conflict via the attempt runner in `interpreter/stm/retry.rs`.
- **Object creation has two explicit placements.** Compile is one shared core
  (`compile_to_process` in `util/process_builder.rs`, reached through
  `compile_process_from_path` / `compile_process_from_code`). Placement is either
  *physical* (`ObjectSpace::insert_process_physical`, blind, no cell — bootstrap and
  test fixtures only, reached through `ObjectSpace::create_process_from_code` and
  the `Vm` forwarder of the same name) or *transactional*
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
