# Command rules

`command::registry` owns rule registration, removal, selection, and storage.
Its transaction-bound `ActorRules` and `VerbRules` views use the existing
per-living and shared parser cells. Efuns decode LPC arguments and resolve
handlers; presence chooses affected objects and runs movement hooks; trial
chooses when to query each registry and executes the resulting candidates.

## Registration and removal

Registration records a blind merge, so concurrent registrations do not conflict
merely because they target the same rule cell. The committer remains the sole
validation and serialization authority. Removal that returns an ownership
answer or count reads the transaction-visible rules and retains that dependency.
Movement cleanup and clearing do not add rule reads.

Each `add_action` verb receives a distinct identity. Native leading alternatives
share an identity, so removing one removes every alternative and `remove_action`
counts them all. Parser synonyms receive fresh identities while retaining the
original base verb used to choose handlers and remove the registration.

Actor candidates are filtered for owner liveness and scope, then sorted by
descending identity. Concurrent registrations can commit out of identity order,
so reversing the stored list would give different precedence. Parser candidates
and inspection follow stored registration order. Parser selection happens only
after actor candidates decline, preserving parser edits made by those handlers
and avoiding a global parser read for successfully handled actor commands.

## Snapshots and ordered edits

`RuleList` is an opaque immutable snapshot backed by `Arc<Vec<Rule>>`. It exposes
slice inspection through `Deref`, supports `Default`, and can be constructed from
a vector or iterator. It replaces the previous public `Arc<[Rule]>` alias;
Rust callers that depended on that representation must use the snapshot API.

The registry's internal `RuleEdit` batches adjacent appends and applies removals
and owner retention in their recorded positions. Generic STM delegates to it
both at commit and when a read materializes pending merges. The first edit to a
shared payload copies its entries; subsequent edits reuse a uniquely owned
vector. A retained snapshot remains unchanged and can require a later copy.

For R existing rules and K adjacent appends, the previous implementation copied
`K*R + K*(K-1)/2` existing entries. The new implementation copies the shared
R-entry payload once and appends K entries. A single append in each of several
commits still copies the shared list each time; the representation does not
provide persistent-sequence insertion costs or dispatch indexes. Reused vectors
can retain spare capacity after growth or removal, trading memory for fewer
allocations; this benchmark does not measure peak memory.

Rule owners remain weak references. GC continues to trace handler pointers and
captured values from the committed rule payload. Inspection retains its existing
liveness distinctions, including `commands()` reporting a dropped owner as zero.

## Measure changes

From the repository root:

```sh
cargo run --release --example command_rule_latency -- 10
cargo run --release --example room_entry_latency -- 20 rules
```

The rule example exercises action, native, and parser registrations with 0 or
256 initial entries and 1, 16, 64, or 256 new entries. It includes blind appends,
a missing-rule observation after each append, alternating registration/removal,
and removal of all matching entries. Object compilation and initial population
are outside timing. First use of a native/parser pattern remains timed when
the list starts empty; later samples in the same process use the warm cache.
It checks final counts, owners, unchanged prior snapshots, zero conflicts, and
one owning attempt per sample, and reports whole-apply and commit-service times.

The room example adds 1, 4, or 16 rules per `init()` at 16 and 64 simultaneous
arrivals, plus source-room cleanup with retained snapshots and unrelated
background applies. See [room-entry latency](room-entry-latency.md) for its
committed-work checks and controls.

## Measured comparison

Measured on 2026-09-14 against `3bf06ef4`, which already includes coordinated
room-entry retries. Both versions used identical benchmark source, release
builds, default features, eight Tokio workers, and workspace configuration on
an Intel Core i7-13700H, Linux x86_64, Rust 1.98.1. Runs alternated which version
went first, with no concurrent builds or tests.

The rule benchmark used ten pairs of process launches, discarding one complete
matrix per launch to warm pattern caches and then measuring two matrices.
That gives twenty samples per case, 1,560 measured samples per version, plus
780 warmup samples. Every sample completed one owning attempt with zero
conflicts and passed the rule-count, owner, and retained-snapshot assertions.

The following medians are microseconds, **before → after**, with 256 existing
rules. Commit service includes validation and application of the changes.

| Surface | Operation | Whole apply (µs) | Commit service (µs) |
|---|---|---:|---:|
| action | Append 1 | 28.8 → 32.6 | 13.8 → 16.9 |
| native | Append 1 | 31.4 → 34.7 | 16.5 → 17.8 |
| parser | Append 1 | 30.7 → 31.9 | 14.6 → 16.7 |
| action | Append 256 | 3222.1 → 124.8 | 3113.0 → 26.3 |
| native | Append 256 | 4324.3 → 126.4 | 4202.9 → 30.3 |
| parser | Append 256 | 3300.7 → 144.7 | 3133.5 → 27.9 |
| action | Append 256, observing each | 2332.0 → 156.9 | 7.2 → 3.7 |
| native | Append 256, observing each | 3214.4 → 160.5 | 11.8 → 3.4 |
| parser | Append 256, observing each | 3685.0 → 1621.4 | 7.3 → 9.7 |
| action | Register/remove 256 times | 3712.6 → 807.9 | 15.2 → 8.4 |
| native | Register/remove 256 times | 5105.8 → 748.3 | 10.8 → 11.8 |
| parser | Register/remove 256 times | 4206.5 → 1119.8 | 8.0 → 11.3 |
| action | Remove all 256 | 1346.5 → 49.7 | 1272.8 → 13.6 |
| native | Remove all 256 | 1713.0 → 50.5 | 1642.7 → 16.7 |
| parser | Remove all 256 | 1384.8 → 41.1 | 1300.4 → 13.9 |

Bulk registration and removal improve substantially. A single blind append is
a measured tradeoff: about 1–4 µs slower here, roughly 4–13% with 256 existing
entries. Single action registration into an empty list rose from 20.2 to
23.7 µs (18%). These small-operation costs were not eliminated by batching;
the design still copies a shared list on its first edit. Repeated observations
also retain their scanning cost, particularly for parser rules.

The room matrix uses twenty paired samples for each of 23 scenarios. Times
below are milliseconds, **before → after**, and are medians across samples,
including medians of each sample's per-task p95 rather than pooled percentiles.

| Scenario | Burst wall (ms) | Task p95 (ms) |
|---|---:|---:|
| shared_1 | 0.209 → 0.203 | 0.170 → 0.170 |
| shared_4 | 0.341 → 0.322 | 0.300 → 0.276 |
| shared_16 | 1.760 → 1.745 | 1.711 → 1.700 |
| shared_64 | 17.586 → 16.916 | 16.058 → 15.694 |
| separate_1 | 0.091 → 0.082 | 0.061 → 0.060 |
| separate_4 | 0.113 → 0.115 | 0.072 → 0.076 |
| separate_16 | 0.197 → 0.186 | 0.142 → 0.138 |
| separate_64 | 0.705 → 0.661 | 0.512 → 0.451 |
| nonliving_64 | 0.483 → 0.497 | 0.327 → 0.335 |
| source_64 | 18.481 → 19.131 | 16.953 → 17.651 |
| rules_64 | 21.938 → 22.440 | 19.364 → 20.072 |
| rules1_16 | 2.142 → 2.010 | 2.035 → 1.909 |
| rules4_16 | 2.937 → 2.670 | 2.875 → 2.584 |
| rules16_16 | 16.368 → 4.139 | 16.329 → 4.096 |
| rules4_64 | 71.936 → 32.448 | 63.443 → 28.927 |
| rules16_64 | 308.379 → 77.594 | 301.176 → 69.694 |
| rules4_source_64 | 85.668 → 46.530 | 77.100 → 43.107 |
| rules4_background_64 | 72.612 → 33.373 | 63.637 → 30.047 |
| shared_hook_64 | 26.171 → 24.014 | 24.198 → 22.076 |
| staggered_64 | 16.619 → 19.284 | 9.363 → 11.685 |
| shared_background_64 | 14.576 → 15.264 | 13.021 → 13.826 |
| staggered_background_64 | 17.384 → 18.592 | 10.038 → 10.924 |
| population_128 | 41.579 → 41.881 | 41.464 → 41.783 |

Both versions requested 21,000 movements. The baseline committed 20,640 and
timed out 360; the refactor committed all 21,000 with zero failures or timeouts.
All baseline failures were in `rules16_64`: 920 of 1,280 arrivals completed,
versus all 1,280 after the change. Its baseline latency therefore includes
timeouts and must not be read as an equal-work speedup. Every sample checked
environments, inventories, hook totals, exact per-owner rule counts, and retained
snapshots where requested.

With four registrations per hook and 64 arrivals, burst latency fell 55%
(71.936 → 32.448 ms). Source-room cleanup with retained snapshots fell 46%
(85.668 → 46.530 ms). In the rule-heavy background scenario, independent counter
apply p95 fell from 1.450 to 0.300 ms; there were 727 baseline and 447 refactor
background samples because the bursts had different durations. These are
independent applies, not network command-response measurements.

Staggered and low-contention controls varied between runs. A separate sixty-pair
followup rechecked the five controls below; every requested movement completed
on both versions. This did not reproduce the large staggered slowdown seen in
the twenty-pair matrix. Smaller differences remain within the observed run-to-run
variation; the single-append regression above is an explicit remaining tradeoff.

| Followup control | Burst wall (ms) | Task p95 (ms) |
|---|---:|---:|
| staggered_64 | 21.398 → 21.286 | 13.508 → 13.113 |
| staggered_background_64 | 19.365 → 19.659 | 11.933 → 11.945 |
| shared_background_64 | 17.793 → 18.316 | 16.207 → 16.628 |
| rules_64 | 25.382 → 24.619 | 23.063 → 22.596 |
| nonliving_64 | 0.646 → 0.630 | 0.384 → 0.393 |

Raw CSVs and the alternating-run scripts are kept locally under
`local/architecture/command-rule-{before,after}.csv`,
`command-room-{before,after}.csv`, `command-controls-{before,after}.csv`,
`measure-command-rules.py`, and `measure-command-controls.py`. To compare another
implementation, build both revisions with the same examples, save their binaries
separately, and alternate executions; do not compare different benchmark sources
or run timing alongside a build.

Validation: `cargo build --workspace`, `cargo test --workspace` (3,501 passed,
2 ignored), `cargo clippy --workspace --all-targets -- -D warnings`,
`cargo fmt --check`, and `RUSTDOCFLAGS="-D warnings" cargo doc --workspace` passed.
Coverage includes concurrent blind registrations, ID/commit ordering,
same-transaction reads and edits, owner-authorized removal, synonyms, rollback,
parser edits from declining actor handlers, successful actor commands avoiding
a parser-read conflict, and GC reachability/reclamation of captured rule values.
