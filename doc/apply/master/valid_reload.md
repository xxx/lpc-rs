# valid_reload

`int valid_reload(string target, object caller, string program)`

Authorize updating the configured master, simul-efun object, or both at runtime.
`target` is `"master"`, `"simul_efun"`, or `"both"`; `caller` is the requesting
object; `program` is the source file defining the requesting code, including `.c`,
or 0 for an efun pointer without a defining program.

The active master is asked for both fresh-state `request_system_reload` and
state-preserving `request_object_recompile` system targets, including object
arguments. It is asked when requested and again in the job's transaction before
preparation, with the original command giver
(`this_player()`). Returning zero, omitting the hook, or
throwing refuses the operation. The replacement cannot authorize itself.
Authorization during preparation continues to use the current master and its
current simul-efun functions until publication commits. During in-place system
recompilation, permission applies also keep their old global layouts and values
while the new initializers run; their writes remain transactional. A system
recompilation additionally asks `valid_recompile` for each selected prototype,
after this hook allows the system update. This hook alone permits only restart.

```c
int valid_reload(string target, object caller, string program) {
    return program == "/secure/admin/reload.c";
}
```

Restrict this permission to trusted administration code: it replaces the code
that defines the driver's security policy. File/include/inherit access performed
during preparation retains the usual checks; `valid_reload` authorizes compiling
the configured top-level source, so no additional `valid_load` is asked for it.

### See also

`request_system_reload`, `query_system_reload`, `request_object_recompile`,
`valid_recompile`, `valid_read`, `valid_inherit`
