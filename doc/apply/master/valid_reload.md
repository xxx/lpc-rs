# valid_reload

`int valid_reload(string target, object caller, string program)`

Authorize replacing the configured master, simul-efun object, or both at runtime.
`target` is `"master"`, `"simul_efun"`, or `"both"`; `caller` is the requesting
object; `program` is the source file defining the requesting code, including `.c`,
or 0 for an efun pointer without a defining program.

The active master is asked when `request_system_reload` is called and again in
the job's transaction before preparation, with the original command giver
(`this_player()`). Returning zero, omitting the hook, or
throwing refuses the operation. The replacement cannot authorize itself.
Authorization during preparation continues to use the current master and its
current simul-efun functions until publication commits.

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

`request_system_reload`, `query_system_reload`, `valid_read`, `valid_inherit`
