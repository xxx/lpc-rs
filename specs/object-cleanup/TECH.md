# Idle object cleanup implementation

## Context

At `62d4723b0d75c85eeca5f0d497cc6f7dea011135`, `GlobalState::gc`
(`src/interpreter/vm/global_state.rs`) roots all resident objects.
`Vm::run` schedules GC separately from LPC tasks, and `TaskSeed` reconstructs
entry calls for STM retries. `Process::world_var_ids` roots structural cells.
See [PRODUCT.md](PRODUCT.md) for the apply contract.

## Proposed changes

- Store optional cleanup metadata only on processes whose programs define the
  hook: monotonic activity/query times, a per-instance in-flight flag, and a
  transactional opt-out cell. A short metadata mutex does not serialize LPC
  execution or replace the committer.
- Refresh activity at call-stack entry across object boundaries. Exempt the
  cleanup target within its automatic task so retries do not invalidate their
  own idle check. No transactional activity writes enter ordinary calls.
- Add a cleanup seed entry that rechecks liveness, initialization, idleness,
  and opt-out in each attempt, constructs the reference-count argument, and
  writes opt-out after a zero result in the same transaction.
- Run one background sweep at a time with sequential bounded evaluations;
  collect weak object references before awaiting to release map guards and
  avoid keeping retired objects alive. A per-instance lease releases the
  in-flight flag and postpones the next query even on error or cancellation.
- Cancel and join the sweep on shutdown. Use the existing runtime error
  reporting path. Keep GC unchanged except rooting the new structural cell.
- Expose configuration and the self-only `request_clean_up` efun; document
  the lifetime and reference-count differences from other LPC drivers.

Activity times are advisory eligibility metadata. Mudlib safety decisions
must read transactional state (for example `all_inventory`) so concurrent
movement invalidates an unsafe destruction attempt.

## Testing and validation

`src/interpreter/vm/clean_up/tests.rs` uses clock advancement and real LPC
applies to cover configuration, idle delay, cross-object activity, repeated
queries, system exclusions, and clone counts (behavior 1–4). Zero/void opt-out,
re-enabling, and aborted requests cover behavior 5–6. Error, timeout, and
authorization tests cover behavior 7–8. GC, stale-instance, reservation, and
VM-loop shutdown tests cover behavior 9.

`src/interpreter/task/clean_up_tests.rs` rejects an attempted destruction after
an arrival commits, then reruns the hook and preserves the occupied room. It
also checks that renewed activity suppresses a conflict retry (behavior 3, 7).
The end-to-end disk-backed fixture inherits its cleanup hook, unloads an idle
room, reclaims its state with GC, and loads a fresh instance on the next visit.

Run workspace build, tests, clippy, formatting, and rustdoc with warnings denied.
