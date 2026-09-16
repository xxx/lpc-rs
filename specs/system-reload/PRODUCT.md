# Runtime system-object reload

## Summary

Administrators can replace the master, simul-efun object, or both while the driver
continues serving work. Failed reloads preserve the active objects.

## Behavior

1. `request_system_reload(target)` accepts `"master"`, `"simul_efun"`, or `"both"`
   and returns a positive request ID. Invalid targets and a missing configured
   simul-efun source raise errors. The request starts only after the caller commits.
2. The active master's `valid_reload(target, caller, program)` must allow the
   request. Missing hooks deny it. Authorization is repeated when the job runs,
   preserving the original requesting object, defining program, and command giver.
3. `query_system_reload(id)` returns a mapping with `target`, `state`, and `error`,
   or 0 for an unknown request. States are `queued`, `running`, `succeeded`, and
   `failed`. Only the requesting object can inspect the request. The most recent
   128 completed requests are retained; IDs from aborted attempts may be skipped.
4. Reload uses the configured source paths, fresh globals, and normal initializers.
   Targets and replacements must be detached daemons without connections,
   environments, inventory, commands, parser registration, or shadow history.
   The old master and its old simul-efuns authorize preparation. Replacing both
   prepares the simul-efun first and compiles the new master against it. Publication is atomic.
5. Compilation, authorization, compatibility, initialization, and timeout failures
   discard all preparation changes and effects and preserve both active objects.
6. Existing simul-efun exports must retain their argument and return types,
   argument counts/default counts, reference parameters, and function flags.
   New exports and changed bodies are allowed. Existing callers and name-based
   simul-efun pointers dispatch to the replacement. Inherited copies are unchanged.
7. An attempt observes system objects through its transaction. Replacement
   invalidates dependent attempts; retries select the new objects and functions.
8. Old object references and local closures become stale. Scheduled work owned
   by replaced objects is cancelled. State migration is not implicit.
9. Requests made while preparing a reload are refused. A request whose original
   caller or command giver has been retired fails. Replacing a missing resident
   object fails. A shared master/simul-efun path is refused.
10. Successful compilation cannot prove policy correctness: administrators must
    retain access to the host to repair source and restart a misconfigured master.
