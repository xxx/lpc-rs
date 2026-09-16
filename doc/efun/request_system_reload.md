# request_system_reload

`int request_system_reload(string target)`

Queue a runtime replacement of the configured `"master"`, `"simul_efun"`, or
`"both"` objects. Returns a positive request ID. The request starts after the
calling transaction commits; an aborted transaction queues nothing. Inspect the
result from a later invocation with `query_system_reload(id)`.

The active master must allow `valid_reload(target, caller, program)`. The driver
asks again when the job runs, using the original requesting object and defining
program and command giver (`this_player()`). A missing hook refuses the request. A retired requester cannot run a
queued reload. The configured simul-efun source must exist for targets involving
it, and every target must already be resident.

The driver compiles and initializes fresh objects in a separate transaction.
The current master and its current simul-efuns authorize includes, inherits, and
operations performed during preparation. Replacing both compiles and initializes
the new simul-efun object first, then compiles the new master against its exports.
The replacements become visible together when preparation commits. New objects
can find themselves during their initialization.

A compilation, permission, compatibility, initializer, or execution-limit error
leaves the current objects active and discards preparation writes, file writes,
output, and scheduled work. Compile warnings use the current warning handler.
A concurrent conflicting change retries preparation; only the successful attempt's
physical effects are delivered.

Existing named simul-efun functions must keep their return and argument types,
argument/default counts, reference parameters, and function flags. Bodies and
default expressions may change, and new functions may be added. Existing compiled
calls and name-based simul-efun pointers use the replacement. Programs inheriting
the source keep their compiled copy; adding an export that shadows an efun affects
newly compiled callers.

Globals start fresh, and normal initializers and `create()` run. State is not
copied. Keep persistent mutable state in separate daemon objects. Both the old
and new objects must be detached daemons: no connection, environment, inventory,
commands, parser registration, or shadow history. Old references and local
closures become stale, and call-outs owned by the retired objects are cancelled.
Reload requests during preparation are refused.

A compiling master can still implement incorrect policy. Keep host access to
repair source and restart if a replacement denies administrative access.

```c
int reload_id;

void update_driver_objects() {
    reload_id = request_system_reload("both");
}

void show_reload_result() {
    mapping result = query_system_reload(reload_id);
    if (result) write(sprintf("%s: %s\n", result["state"], result["error"]));
}
```

### See also

`query_system_reload`, `valid_reload`
