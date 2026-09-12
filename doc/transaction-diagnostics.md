Transaction failures emit one `lpc_rs::transactions` warning for the owning task and attach the same diagnostic to its error notes. Nested applies contribute to the owner's transaction; they do not emit separate transaction summaries. Successful transactions that needed retries emit a debug summary. Set `RUST_LOG` in the driver's environment or `.env`:

```dotenv
RUST_LOG=info,lpc_rs::transactions=debug
```

The summary identifies the owning object and entry point. A player command is labelled `command`; callback resolution and connection binding use `resolve_callback`, `attach`, or `detach`. It does not record argument values or command text. The competing writer's label identifies its owning entry point, which may have made the actual write in a nested apply.

| Field | Meaning |
|---|---|
| `attempts`, `conflicts` | Counts for this owning task across retries. |
| `elapsed_ms` | Total loop wall time, before formatting the diagnostic. |
| `evaluation_and_snapshot_ms` | Attempt time excluding compilation; includes obtaining a snapshot and scheduling delays. |
| `compilation_ms` | Time inside transaction-triggered file compilation, including compiler policy applies; nested compilations count once and cancellation is included. |
| `commit_phase_ms` | Commit preparation, submission, committer queue/service, and receiving the reply, combined. |
| `backoff_yield_ms`, `backoff_sleep_ms` | Realized retry waits in each backoff tier. |
| `delivery_ms` | Delivery of the successful transaction's deferred effects. |
| `last_conflict` | The last rejection encountered before completion or failure; `none` means no commit rejection was observed. |

These are wall times, not CPU measurements. Compilation performed before a root task starts, such as bootstrap compilation, is outside these timings. Diagnostic formatting and object-name lookup are also outside them. Commit and effect delivery remain uncancelled after submission, and asynchronous evaluation limits can overshoot while waiting for the runtime to poll again.

A read invalidation reports `cell`, `base`, `current`, `written_at`, and `writer`. `written_at` is the first newer commit encountered that invalidated a tracked read; it need not be the newest commit. Each rejection reports one conflicting cell, not the full intersection. The last rejection is evidence of a dependency, not necessarily the most frequent hotspot across the task's attempts.

The optional `field` label names a resident object's structural cell or global variable, a program's clone list, a stable object-space path cell, or the parser's verb rules. Array and mapping payloads can name a global alias found in the attempt's snapshot, labelled `via`; that alias is not an exclusive owner. Names are best-effort: removed objects, upvalues, and indirectly referenced payloads may have only a numeric cell ID or payload kind. Name lookup does not add transactional reads.

Other rejection reasons are reported separately:

- `merge type mismatch`: a deferred operation cannot apply to the current cell type.
- `history unavailable`: the base predates retained write history.
- `future base version`: the base is ahead of the committer's current version.

Writer metadata contains names only and expires with the existing write history. Raw/internal commits without an entry label report an unknown writer. Successes without retries do not perform object or payload alias scans; a successful retry does so only when its debug summary is enabled.

For a timeout with no conflicts, inspect compilation and evaluation time first. For a timeout with retries, use `field` and `writer` to identify the shared dependency, then compare discarded attempt work with commit and backoff time. These diagnostics do not change admission, validation, deadlines, or retry behavior.
