# Idle object cleanup

## Summary

The driver periodically asks idle objects to run `clean_up(int references)`.
LPC code decides whether to destroy the object, preserving mudlib control over
room contents, persistent state, and scheduled work.

## Behavior

1. `CLEAN_UP_INTERVAL` sets the minimum idle time in seconds, defaults to 600,
   and disables automatic cleanup when zero. `LPC_CLEAN_UP_INTERVAL` takes
   precedence. Checks run at most once per minute, or once per interval when
   the interval is shorter; deadlines are approximate.
2. Only initialized objects defining `clean_up` are queried. The master and
   simul-efun objects are excluded. The hook runs directly on the object,
   without shadow interception or a command giver.
3. Creation and entry into an object's code start its idle period. Recursive
   calls within that object do not refresh it. Automatic cleanup execution in
   the target does not count as activity. Aborted execution may conservatively
   postpone cleanup. Holding a reference or a future call-out does not prevent
   a query; the mudlib must protect anything it needs to retain.
4. The hook receives zero for a clone. Other objects receive one plus the
   number of live clones sharing their compiled program. This is not a count
   of LPC references or inheriting programs; inherited code has independent
   object lifetimes in this driver.
5. A zero or void return disables future queries for that instance. Nonzero
   returns permit another query after at least the configured interval.
   Returning a value never destroys the object automatically.
6. `void request_clean_up()` enables future queries in the calling object.
   It has no effect on objects without the hook or on a disabled driver
   scheduler. A hook returning zero overrides its own request to re-enable.
7. Each hook executes as an ordinary transaction with the configured evaluation
   limit and conflict retries. Inventory checks, destruction, and disabling
   queries commit together; retries recheck liveness and idleness. Existing
   `valid_destruct` authorization applies.
8. An uncaught error or timeout rolls back the hook, is reported through the
   runtime error handler, and allows another query after the idle interval.
   A failure in one object does not prevent checking other objects.
9. Automatic queries never overlap for the same object. Destroyed instances
   queued for checking are skipped, including when the path has been reloaded.
   Opt-out state survives memory GC. Shutdown cancels pending cleanup work
   before closing the committer.

