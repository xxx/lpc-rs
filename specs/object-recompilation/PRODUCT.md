# Live object recompilation

## Summary

Recompile a loaded prototype and its current clones while preserving each object's
identity and compatible state. Existing objects use the new code after a successful
upgrade; failed preparation leaves the entire group unchanged.

## Behavior

1. An explicit administrative operation upgrades a live, initialized prototype
   and the live clones sharing its compiled program. Ordinary destruct/load keeps
   its existing semantics. Older surviving program generations and inheriting
   programs are not implicitly upgraded.
2. Authorization is required from the active master; a missing permission apply
   denies the operation. Compilation retains the normal include, inherit, source
   access, warning, and execution-limit checks.
3. The source is the prototype's backing file. A clone, virtual instance,
   destructed object, uninitialized object, master, or simul-efun object cannot
   be the target. Missing or invalid source fails without replacing anything.
4. All upgraded objects keep their references, names, creation times, connections,
   command state, environments, and inventories. State is individual to each
   object; no clone receives another clone's globals.
5. Globals retain their values when their declaring source, name, and declared
   type match. This includes private and inherited declarations. Renamed,
   removed, or type-changed declarations do not carry values forward. Arrays,
   mappings, object references, and function values retain their identity.
6. Global initializers run against the new code for each upgraded object, then
   compatible old values overwrite their initialized values. New and type-changed
   variables keep their initialized values, or zero when no initializer exists.
   `create()` is not called. Initializer side effects are transactional.
7. Publication is atomic for the whole group. Compilation, authorization,
   initialization, compatibility, or execution-limit failure discards preparation
   changes and effects. Concurrent global writes, cloning, destruction, and another
   upgrade cannot produce a partially upgraded group or lose committed state.
8. Running work observes consistent code and globals. Work invalidated by an
   upgrade retries against the new program. Newly created clones use the new
   program after publication.
9. Local function pointers and anonymous functions made before the upgrade of
   their receiver become stale and raise an explicit error when called. Dynamic,
   efun, and name-based simul-efun pointers retain their normal behavior. Pending
   callbacks are retained, but a callback holding a stale local pointer fails
   when invoked; callers must recreate such pointers after upgrading.
10. Clone enumeration reports the upgraded group consistently from either the
    prototype or a clone. An old generation still enumerates its own group.
11. The first version refuses objects participating in a shadow chain and changes
    to cleanup eligibility; this avoids silently violating previously established
    shadow permissions or changing scheduler registration. A nested upgrade during
    preparation is refused.

## Request interface

12. `request_object_recompile(object prototype)` returns a positive request ID.
    The job starts after the caller commits; an aborted transaction queues nothing.
    The active master's `valid_recompile(prototype, caller, program)` must allow
    both the request and the job. The original requester and command giver must
    still be live. Concurrent requests for one prototype run against its version
    at preparation time and serialize through transaction validation.
13. `query_object_recompile(int id)` returns 0 for an unknown request or a mapping
    containing `target`, `state`, `error`, and `updated`. States are `queued`,
    `running`, `succeeded`, and `failed`. Only the requesting object may inspect
    the request. `updated` includes the prototype and is zero unless successful;
    `error` is empty unless failed. A destructed target appears as zero. The most
    recent 128 completed requests are retained; aborted IDs may be skipped.
