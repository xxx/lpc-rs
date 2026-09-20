# Live object recompilation implementation

## Context

Baseline: `99fcf61dac17bba6c227678a982ddac8ff11f711`. See [PRODUCT.md](PRODUCT.md)
for behavior. `Process` currently combines stable object identity with an immutable
program and fixed global-cell layout. `CallFrame` resolves global indices through
that process; local pointers retain code and the original layout. Clone membership
already lives in a transactional array on `Program`.

Relevant modules are `src/interpreter/process/mod.rs`, `program.rs`,
`call_frame.rs`, `function_type/`, `task/`, `stm/`, `vm/object_update.rs`,
`vm/object_recompile.rs`, and `vm/system_recompile.rs`.

## Implementation

- Introduce an immutable program image containing the program, global-cell layout,
  and a generation identity. Each process has a transactional image binding;
  an immutable initial image supports bootstrap and freshly constructed objects.
- Resolve runtime program inspection and dispatch through that binding. Frames
  pin the image selected by their transaction, keeping global reads direct.
  Local calls share a receiver binding containing identity and image, retaining
  the 64-byte frame and its existing ownership cost. No physical pointer swap can
  bypass STM.
- Resolve named pointers and driver callback entries against the attempt's image,
  retaining declaration identity and checking interface compatibility across
  generations. Anonymous pointers retain their original image and captures;
  nested local execution shares that image, while external calls use the current
  image. Closures created by retained code inherit its image. Driver-generated
  composition executors remain independent of user program generations.
- Extend world tracing to follow current images and their global cells. Retired
  layouts cease to root their globals once older attempts and reachable closures
  release them. Keep compiler-only access to initial programs explicit.
  Each process retains its construction image as an immutable fallback until
  destruction; later retired images are released with their last snapshot, frame
  or anonymous function. Trace retained images' globals from reachable pointers
  so removed variables remain usable until the last retaining closure is gone.
  Trace each image once per collection to bound the work queue when many globals
  contain closures retaining the same image.
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
- Preserve scheduled work and repeat intervals, resolving named callback entries
  again after a concurrent upgrade and retaining anonymous code and captures.
  Refuse active shadow chains and changes to the construction-time cleanup
  eligibility in this first version.

## Testing and validation

- Exercise authorization, invalid targets, request lifecycle, and source errors
  through the LPC surface (behavior 1–3).
- Verify prototype and independent clone state, stable object identity, position,
  references and creation times; test reordered, added, removed, type-changed,
  inherited, private, and duplicate-name globals (behavior 4–6).
- Fail after initializer writes and effects, and verify complete rollback; hold
  an old attempt across publication and verify conflict and refreshed execution;
  race global mutation, cloning and destruction (behavior 7–8).
- Exercise named callback rebinding, private and inherited declaration identity,
  incompatible definitions, retained closures and their globals after GC,
  repeating and prepared callbacks, composition, clone enumeration, repeated
  upgrades and old program groups (behavior 9–10).
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

## System upgrades and job orchestration

Use one recompilation request, registry, committed effect, VM operation and
attempt body for ordinary and system targets. `valid_recompile` authorizes each
selected prototype at request and execution time. There is no separate system
restart operation or permission hook.

Resolve master and simul-efun objects transactionally, with named driver entries
and simul callbacks resolving their programs on every attempt. Bootstrap publishes
object cells transactionally; retired initializers cannot reclaim a replaced path.
System preparation uses the same object identities throughout, with an authority
view selecting their previous program images for permission applies.

Compiler simul-efun input is an explicitly selected `Arc<Program>`, never a
construction-time program inferred from a live process. System preparation pins
old master/simul images in an authority transaction view. For upgraded system
objects, that view redirects global slots to private cells seeded with the old
values; new initializers use the staged image and ordinary cells. Permission
applies share the transaction's reads, writes and effects but use the pinned view.
Authority writes also reach their original cells; after each group initializes,
restore its retained globals from the authority cells, preserving old-policy
writes and compatible cell identities. All views are attempt-local.

Validate identity/state retention, paired publication, repeated upgrades, current
compiler exports, old-policy code and state, rollback, permissions, requester
provenance, callback continuity and conflicts with running work and other upgrades.

### System upgrade validation

Before removal of the separate restart API, commit `e1188c4b` passed the workspace
suite (3,733 tests; two existing ignored tests), strict all-target Clippy,
formatting, warning-free rustdoc and the
ulib checks. Eleven additional scenarios cover paired publication, preserved
identity and clone state, skipped `create`, current compiler exports, repeated
upgrades, old authorization code and globals, permission-apply counter writes,
rollback, captured global-cell identity, permissions, conflicting restart, old
attempt invalidation, callout retention and retirement during a paired initializer.

Repeated `fib(20)` comparisons used the preserved `68ae16dd` release binary and
`e1188c4b`, with no concurrent builds and all threads pinned to one CPU:

| CPU | Runs per version | Warmup / measurement | Before system upgrades | After |
| --- | --- | --- | --- | --- |
| Performance core, CPU 4 | 4 | 2 s / 4 s | 1.766 ms | 1.762 ms |
| Efficiency core, CPU 12 | 2 | 3 s / 5 s | 2.471 ms | 2.503 ms |

Each invocation collected 40 samples; values are medians of invocation means and
version order was alternated and reversed. The performance-core result is
unchanged; the efficiency-core difference is 1.3% in this short comparison.
These runs do not reproduce the earlier 10% recursion regression and are not a
production throughput guarantee.

### Single recompilation interface validation

After removing the separate restart API, `cargo test --workspace --no-fail-fast`
passes 3,725 tests with two existing ignored tests. Explicit ulib checks, strict
all-target Clippy, formatting and warning-free rustdoc also pass; Cargo retains
the existing `sha1` dependency documentation filename warning.

Coverage includes per-prototype system permissions for selectors and object
arguments, caller provenance, concurrent upgrades, incompatible exports, prepared
callbacks, recursive-request refusal and missing system objects. The retired
initializer/GC check now lives alongside bootstrap initialization; destruction
tests continue to cover delayed callout cancellation.

### Callback continuity validation

With callback preservation, `cargo test --workspace --no-fail-fast` passes 3,735
tests with two existing ignored tests. Explicit ulib checks, strict all-target
Clippy, formatting and warning-free rustdoc also pass.

Ten additional scenarios cover repeating timers through multiple upgrades,
anonymous captures and removed/type-changed globals through GC, lexical versus
external calls from retained code, closures created by old code, private inherited
bindings, incompatible interfaces, composition, prepared closures, reclamation
after cancellation, and registered actions. Existing callback tests now verify
updated named functions and preserved anonymous behavior, including system objects.
Ordinary recursive calls retain the existing frame and receiver-sharing path;
the performance measurements above apply to their explicitly recorded revisions.
