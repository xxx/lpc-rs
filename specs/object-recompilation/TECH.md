# Live object recompilation implementation

## Context

Baseline: `99fcf61dac17bba6c227678a982ddac8ff11f711`. See [PRODUCT.md](PRODUCT.md)
for behavior. `Process` currently combines stable object identity with an immutable
program and fixed global-cell layout. `CallFrame` resolves global indices through
that process; local pointers retain code and the original layout. Clone membership
already lives in a transactional array on `Program`.

Relevant modules are `src/interpreter/process/mod.rs`, `program.rs`,
`call_frame.rs`, `function_type/`, `task/`, `stm/`, and `vm/system_reload.rs`.
The existing system reload provides the compilation and attempt-runner pattern,
but replaces object identities and initializes fresh state.

## Implementation

- Introduce an immutable program image containing the program, global-cell layout,
  and a generation identity. Each process has a transactional image binding;
  an immutable initial image supports bootstrap and freshly constructed objects.
- Resolve runtime program inspection and dispatch through that binding. Frames
  pin the image selected by their transaction, keeping global reads direct.
  Local calls share a receiver binding containing identity and image, retaining
  the 64-byte frame and its existing ownership cost. No physical pointer swap can
  bypass STM.
- Tag local pointers and driver callback entries with their generation, and check
  it whenever execution starts or retries. Named entries resolve anew per attempt.
- Extend world tracing to follow current images and their global cells. Retired
  layouts cease to root their globals after older attempts are released. Keep
  compiler-only access to initial programs explicit.
  Each process retains its construction image as an immutable fallback until
  destruction; later retired images are released with their last snapshot or frame.
- Compile once per upgrade attempt and build a declaration migration map from
  `global_variable_info`, keyed by declaring source, name, and exact declared type.
  Stage images for every group member, reusing compatible global cell identities,
  run each layout's global initializer
  without the program initializer's `create()`, and restore retained values.
- Track prototype identity, group membership, member liveness and images, and
  retained global values. Publish the images and clone membership in one changeset.
  Validate that initializers did not destruct or otherwise invalidate targets.
- Reuse the existing compilation gate and transactional source reader. Keep
  upgrade authorization explicit and denied by default. Update its reference,
  ulib example and reviewed apply record together.
- Preserve scheduled work and reject stale local code at dispatch, including
  callbacks resolved before a concurrent upgrade. Refuse active shadow chains and
  changes to the construction-time cleanup eligibility in this first version.

## Testing and validation

- Exercise authorization, invalid targets, request lifecycle, and source errors
  through the LPC surface (behavior 1–3).
- Verify prototype and independent clone state, stable object identity, position,
  references and creation times; test reordered, added, removed, type-changed,
  inherited, private, and duplicate-name globals (behavior 4–6).
- Fail after initializer writes and effects, and verify complete rollback; hold
  an old attempt across publication and verify conflict and refreshed execution;
  race global mutation, cloning and destruction (behavior 7–8).
- Exercise local and anonymous pointer staleness, dynamic calls, pending callbacks,
  clone enumeration, repeated upgrades and old program groups (behavior 9–10).
- Check shadow and cleanup refusals, GC after migration, and compilation/runtime
  introspection against the selected image (behavior 11 and runtime invariants).
- Run workspace tests, all-target clippy, formatting, and warning-free docs;
  include the ulib signature and apply-review checks. Measure representative
  interpreter workloads if the frame or dispatch changes regress performance.

## Validation results

Verified on 2026-09-19:

- `cargo test --workspace --no-fail-fast`: 3,722 passed, two pre-existing ignored
  tests, including 14 recompilation scenarios and the ulib checks.
- `cargo clippy --workspace --all-targets -- -D warnings` and `cargo fmt --check`
  passed.
- `RUSTDOCFLAGS="-D warnings" cargo doc --workspace` passed; Cargo still reports
  the existing dependency documentation filename collision between two `sha1`
  packages.
- Release benchmarks compared with the baseline commit on the same development
  machine, with compilation stopped: `bench_fib` used 30 samples and
  `bench_inheritance` used 20 samples per case, each with one second of warmup
  and two seconds of measurement. The ten inheritance cases had
  no detected regressions; eight improved and two had no significant change.
  An initial unpinned `fib(20)` comparison suggested a 9.6% regression, but the
  controlled comparison below did not reproduce it; it is not an established
  cost of recompilation support.

### Controlled recursion comparison

Compared separate release binaries built from `99fcf61d` and `68ae16dd`, with
identical build settings, no concurrent compilation, and all benchmark threads
pinned to the same CPU. Each invocation collected 40 samples; version order was
alternated and reversed to expose run-to-run variation.

| CPU | Independent runs per version | Warmup / measurement | Baseline | Recompilation |
| --- | --- | --- | --- | --- |
| Performance core, CPU 4 | 4 | 1 s / 3 s | 2.015 ms | 2.008 ms |
| Efficiency core, CPU 12 | 2 | 3 s / 5 s | 2.556 ms | 2.525 ms |

Values are medians of per-invocation mean times. The performance-core baseline
ranged from 1.965 to 2.334 ms, while the new implementation ranged from 2.001 to
2.014 ms. The earlier single unpinned comparison understated this variation.
These results do not support a repeatable 10% recursion regression, and do not
establish a general performance improvement either.

Local recursion keeps the existing 64-byte frame and two retained references per
call (code and receiver). It shares the receiver's pinned image instead of reading
the transactional image binding or allocating a receiver on every local call.
