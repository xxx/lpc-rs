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
operation. For system targets (including object arguments), the driver first asks
`valid_reload` with `"master"`, `"simul_efun"`, or `"both"`. A paired upgrade then
asks this hook for the simul-efun prototype and the master prototype, in that
order. Both hooks are required and both are checked again during preparation.
A retired requester or command giver cannot run the queued job.

Permission authorizes compilation of the backing source; `valid_load` is not also
asked for the top-level source. Includes and inherits retain their ordinary
`valid_read` and `valid_inherit` checks. The driver separately rejects invalid
targets and incompatible runtime conditions. Initializers run transactionally
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

`request_object_recompile`, `query_object_recompile`, `valid_reload`, `valid_read`,
`valid_inherit`
