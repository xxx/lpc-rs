# Room-entry latency

The VM's shared attempt runner queues retries after read invalidations, granting
one retry turn per conflicting cell. First attempts run immediately. The turn
covers snapshot acquisition through the commit reply and ends before effect
delivery. Nested applies share their owner's turn, and queue waiting consumes
the original execution allowance. The committer still validates every attempt.

See [transaction diagnostics](transaction-diagnostics.md) for the policy and
[transaction_stats](efun/transaction_stats.md) for admission counters.

## Reproduce the workload

From the repository root:

```sh
cargo run --release --example room_entry_latency -- 20
cargo run --release --example room_entry_latency -- 20 shared_64
```

The optional second argument filters scenario names by substring. The example
prints CSV. Each sample creates a fresh VM, warms room and NPC programs, and
finishes fixture setup before timing. It uses eight Tokio workers and the same
300 ms execution allowance for timed arrivals in every scenario; fixture
initialization has a separate 3,000 ms allowance. A barrier releases the independent
owning tasks together. Per-task latency starts immediately before its apply;
burst wall time also includes scheduling from barrier release to completion.

Every sample checks committed destination inventory, each occupant's environment,
and exact committed init-hook totals. The command-rule scenarios also check
that every occupant has exactly the expected number of each neighbour's rules. Failed tasks, timeouts,
requested movements, and committed movements are reported separately. The
population scenario has eight owners, each cloning and moving sixteen NPCs;
other scenarios have one movement per owner.

Rule scenarios vary registrations per hook (1, 4, 16) and simultaneous arrivals
(16, 64). `rules4_source_64` moves actors from another populated room while
retaining their previous rule snapshots, checking both cleanup and snapshot
immutability. `rules4_background_64` also runs independent counter applies.
The rule storage design and a registration/removal microbenchmark are described
in [command rules](command-rules.md).

The staggered scenarios release half the owners immediately and schedule the
rest at 200 microsecond intervals, subject to timer rounding. Background
scenarios repeatedly run an independent counter action while arrivals remain
in flight, with a 500 microsecond sleep between actions. Background p95 measures
these actions separately; their sample counts vary with burst duration.

## Measured comparison

Baseline: commit `5760b740`, before retry admission. Both release binaries used
identical example source, default features, and workspace configuration. The
baseline was built in a detached worktree; binaries were saved separately.
Twenty pairs of runs alternated which version ran first, with no concurrent
builds or test runs. Hardware: Intel Core i7-13700H, Linux x86_64; Rust 1.98.1.

Each cell below is **before → after**. Times are milliseconds. Values are
medians across twenty samples, including medians of each sample's per-task
percentiles; they are not pooled percentiles. Conflicts are per owning task.

| Scenario | Burst wall | Task p50 | Task p95 | Task p99 | Conflicts/owner |
|---|---:|---:|---:|---:|---:|
| shared_1 | 0.203 → 0.216 | 0.184 → 0.194 | 0.184 → 0.194 | 0.184 → 0.194 | 0.00 → 0.00 |
| shared_4 | 0.327 → 0.326 | 0.137 → 0.141 | 0.284 → 0.269 | 0.284 → 0.269 | 1.00 → 0.75 |
| shared_16 | 4.386 → 1.800 | 0.873 → 0.682 | 4.352 → 1.724 | 4.352 → 1.724 | 4.34 → 0.94 |
| shared_64 | 68.144 → 18.599 | 16.001 → 5.239 | 62.712 → 16.965 | 67.656 → 18.291 | 8.46 → 0.97 |
| separate_1 | 0.100 → 0.098 | 0.075 → 0.064 | 0.075 → 0.064 | 0.075 → 0.064 | 0.00 → 0.00 |
| separate_4 | 0.123 → 0.112 | 0.068 → 0.057 | 0.088 → 0.087 | 0.088 → 0.087 | 0.00 → 0.00 |
| separate_16 | 0.191 → 0.192 | 0.079 → 0.084 | 0.117 → 0.123 | 0.117 → 0.123 | 0.00 → 0.00 |
| separate_64 | 0.621 → 0.639 | 0.236 → 0.270 | 0.410 → 0.436 | 0.454 → 0.472 | 0.00 → 0.00 |
| nonliving_64 | 0.466 → 0.473 | 0.187 → 0.206 | 0.310 → 0.298 | 0.338 → 0.364 | 0.00 → 0.00 |
| source_64 | 71.578 → 20.134 | 19.898 → 6.636 | 64.225 → 18.480 | 71.458 → 19.944 | 9.78 → 1.02 |
| rules_64 | 73.351 → 22.261 | 19.166 → 6.357 | 63.976 → 20.083 | 72.950 → 22.064 | 8.64 → 0.97 |
| shared_hook_64 | 81.201 → 27.479 | 19.500 → 8.264 | 73.011 → 25.325 | 80.981 → 27.363 | 8.46 → 1.38 |
| staggered_64 | 65.647 → 17.675 | 13.020 → 5.516 | 56.645 → 10.204 | 63.663 → 12.783 | 7.05 → 0.98 |
| shared_background_64 | 62.450 → 15.464 | 15.335 → 4.670 | 57.822 → 14.115 | 62.379 → 15.213 | 9.02 → 0.97 |
| staggered_background_64 | 58.567 → 18.601 | 12.539 → 5.191 | 51.974 → 10.642 | 57.362 → 14.925 | 7.41 → 0.98 |
| population_128 | 109.138 → 46.972 | 38.902 → 13.590 | 109.072 → 46.799 | 109.072 → 46.799 | 3.50 → 0.88 |

Each version completed all 14,920 requested movements across 12,520 owning
arrival tasks, with zero failures or timeouts; background work is counted separately. The shared 64-arrival burst's median
wall time fell 73%, and its median per-task p95 fell 73%. The historical
128-NPC population workload improved from 109.1 ms to 47.0 ms.

For background work, the median sample p95 fell from 1.045 ms to 0.200 ms during
simultaneous arrivals and from 1.022 ms to 0.261 ms during staggered arrivals.
Independent-room and non-living controls remained below a millisecond: the
separate-64 median task p95 moved from 0.410 ms to 0.436 ms, a 26 microsecond
increase; non-living p95 moved from 0.310 ms to 0.298 ms.

These are local measurements of warmed synthetic workloads, not production
latency guarantees. First-attempt timing and small controls include scheduler
noise. The arrival streams are finite. Fresh attempts can still invalidate an
admitted retry, and different conflicting cells can send related work to
different queues; FIFO admission does not guarantee completion or output order
under arbitrary continuing traffic.

## Control regression follow-up

Four control scenarios were repeated for 100 pairs each using the preserved
release binaries, alternating version order and running without concurrent
builds or tests. The table shows medians across samples in microseconds,
**before → after**; task percentiles are calculated within each sample first.

| Scenario | Burst wall | Task p50 | Task p95 |
|---|---:|---:|---:|
| shared_1 | 193.0 → 195.5 | 175.0 → 177.0 | 175.0 → 177.0 |
| separate_16 | 301.5 → 302.0 | 148.0 → 156.0 | 234.0 → 236.5 |
| separate_64 | 608.5 → 592.5 | 280.5 → 276.5 | 372.0 → 380.5 |
| nonliving_64 | 579.0 → 549.5 | 244.0 → 210.5 | 347.5 → 321.0 |

The separate-64 median sample p95 increase shrank from 26 to 8.5 microseconds;
the single-entry increase shrank from 10 to 2 microseconds. For all four
controls, paired bootstrap 95% intervals for the median differences in wall
time, task p50, p95, and p99 included zero (10,000 resamples). For example,
separate-64 p95's paired median difference was -1.5 microseconds, with an
interval of -24.5 to +18.0 microseconds. Changes also varied in direction
across groups of 25 pairs. No control sample failed or timed out.

These measurements do not establish a repeatable control-case regression, and
they do not establish equivalence. They cover the listed room-entry workloads;
CPU and memory costs were not isolated, and read-heavy retry bursts outside
these scenarios remain unmeasured.
