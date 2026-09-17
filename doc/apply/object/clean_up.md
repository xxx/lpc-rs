# clean_up

`int clean_up(int references)`

The driver calls this apply in initialized objects that have been idle for
`CLEAN_UP_INTERVAL` seconds (default 600). Zero disables automatic queries.
`LPC_CLEAN_UP_INTERVAL` overrides the unprefixed setting. Sweeps run once per
minute, or once per interval when shorter; a busy sweep can delay a query.

Return zero (including a void return) to stop future queries for this instance.
Return nonzero to allow another query after at least the interval. To enable
queries again later, call `request_clean_up()` in that object. A zero return
overrides a request made within the hook. Returning never destroys an object:
the hook must call `destruct(this_object())` if that is appropriate.

The argument is zero for clones, and one plus the number of live clones sharing
the object's compiled program otherwise. Unlike some LPC drivers, this does
not count inheriting programs or arbitrary LPC object references. Inherited
code and its objects have independent lifetimes in lpc-rs.

Creation and entry into an object's code refresh its idle time. Calls within
the same object do not refresh it, and the automatic cleanup task exempts its
target. An aborted call can still postpone cleanup. Merely holding an object
reference or scheduling a future call-out does not keep an object active.
The master and simul-efun objects are never automatically queried. Shadows do
not intercept this hook, and `this_player()` is zero.

The hook runs in a normal transaction under `MAX_EXECUTION_TIME`. Conflict
retries recheck eligibility and rerun the hook. An uncaught error rolls back
its changes, is sent to `error_handler`, and allows a later query. Destruction
still requires the master's `valid_destruct` permission.

For a room whose state is disposable, a conservative starting point is:

```lpc
int clean_up(int references) {
    if (references > 1 || sizeof(all_inventory(this_object()))) {
        return 1;
    }
    destruct(this_object());
    return 0;
}
```

Inventory checks and destruction share a transaction: a concurrent arrival
invalidates an attempt that observed an empty room. Add mudlib-specific checks
for puzzle state, persistent belongings, scheduled events, or anything else
that must survive. Rooms can be loaded again from their source, but unsaved
state from a destroyed instance is lost. Memory GC is separate and never
decides that a resident room is safe to destroy.

### See also

`request_clean_up`, `destruct`, `all_inventory`, `object_clones`, `load_object`
