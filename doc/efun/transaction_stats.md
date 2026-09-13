# transaction_stats

`mapping transaction_stats()`

Return the driver's transaction counters and gauges as a fresh mapping of
integers. This efun requires no authorization and prints nothing.

Counters accumulate for the lifetime of this VM and are not reset by a read.
All durations are nanoseconds. Values too large for an LPC integer saturate at
its maximum positive value.

| Key | Meaning |
| --- | --- |
| `applies` | Finished attempt-runner invocations, including nested applies |
| `attempts` | Execution attempts recorded by those finished invocations |
| `conflicts` | Conflicts recorded by those finished invocations |
| `errors` | Finished invocations that returned an error |
| `total_ns` | Sum of elapsed invocation times, including retries, backoff, and delivery |
| `backoff_yield_ns` | Time spent in the yield backoff tier |
| `backoff_sleep_ns` | Time spent in the sleep backoff tier |
| `backoff_sleep_requested_ns` | Requested sleep time, before early wakes and timer rounding |
| `backoff_commit_wakes` | Sleep waits ended by a commit notification |
| `backoff_cap_expiries` | Sleep waits that ran their full cap |
| `commits` | Successful commits that created a new world version |
| `read_only_commits` | Successful commits with no writes and no new version |
| `commit_conflicts` | Commit requests rejected due to conflicts |
| `reply_failures` | Committer replies whose receiver was gone |
| `busy_ns` | Time the committer spent servicing work; stats reads excluded |
| `commit_service_ns` | Time spent servicing commit requests alone |
| `validation_scanned_versions` | Total history versions scanned during validation |
| `queue_len` | Current committer message backlog (gauge) |
| `queue_peak` | Largest backlog observed behind a dequeued message |
| `evictions` | Write-history versions evicted |
| `live_snapshots` | Currently pinned transaction snapshots (gauge) |
| `versions_retained` | Currently retained write-history versions (gauge) |

These are observational measurements, independent of the calling transaction's
world snapshot. The committer's counters are sampled together, but attempt
telemetry fields and queue length are sampled separately. Concurrent work may
straddle the sample; do not assume exact relationships between fields.

Attempt telemetry is recorded when an invocation finishes, so the current
invocation and other unfinished invocations are excluded. Nested applies that
already finished are included, even if their outer transaction has not committed
or ultimately retries. Consequently, `applies` is not a count of committed
top-level commands, and `errors` may include errors caught by outer LPC code.
`commit_conflicts` includes rejected commits whose retry loop has not yet finished.

Elapsed times overlap across concurrent and nested invocations: `total_ns` is
neither CPU time nor server uptime. `live_snapshots` counts pins, not players or
commands. Creating the result mapping participates in the caller's transaction,
like other returned containers.

Sample twice across separate commands or call outs and subtract counters to
measure rates over that interval; use gauges directly.

```c
dump(transaction_stats());
```

## See also

`query_resident_memory`, `dump`
