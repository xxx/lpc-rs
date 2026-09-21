# valid_recompile

`int valid_recompile(object prototype, object caller, string program)`

Authorize an in-place upgrade of `prototype` and the live clones sharing its
current compiled program. `caller` requested the upgrade; `program` is the source
file defining that request, including `.c`, or 0 for an efun pointer with no
defining source.

The active master is asked by `request_object_recompile` and again in the upgrade
job's transaction before compilation. Both invocations preserve the original
command giver (`this_player()`). `this_object()` is the master; the requesting
object is its caller. A zero result, missing hook or thrown error denies the
operation. System targets use this same hook: a string selector is resolved to
the actual prototype before authorization. A paired upgrade asks for the
simul-efun prototype and the master prototype, in that order, both at request
time and again during preparation. Policies should explicitly control access to
these system prototypes; authorizing ordinary objects must not accidentally
grant permission to change driver policy. A retired requester or command giver
cannot run the queued job.

`query_object_recompile` also asks this hook when the querying object is not the
original requester. In that case `caller`, `program`, `previous_object()` and
`this_player()` describe the current query, and the hook runs inside its
transaction. Nonzero grants access to status, including failure diagnostics;
zero or a missing hook refuses access, and thrown errors propagate. System
selectors resolve to their current prototypes, with `"both"` checking simul-efuns
then master. A destroyed object target or missing system prototype is passed as
0; an object target never switches to a replacement at the same path. Status
access does not require an initialized, recompilable target. The original
requester can read status without this hook, and unknown request IDs return 0
without authorization.

For upgrades, permission authorizes compilation of the backing source;
`valid_load` is not also asked for the top-level source. Includes and inherits
retain their ordinary `valid_read` and `valid_inherit` checks. The driver
separately rejects invalid targets and incompatible runtime conditions.
Initializers run transactionally
under the existing master policy, as each upgraded object, with the requester in
the caller chain and the original command giver. System recompilation pins the
old master and simul-efun code and global layouts for permission applies until
publication; staged new code cannot authorize itself. Permission-apply writes
commit with a successful upgrade and are retained in compatible globals.

```c
int valid_recompile(object prototype, object caller, string program) {
    return program == "/secure/admin/upgrade.c";
}
```

### See also

`request_object_recompile`, `query_object_recompile`, `valid_read`,
`valid_inherit`
