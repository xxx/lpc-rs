# valid_exec

`int valid_exec(string program, object new, object old)`

The driver applies `valid_exec` in the master before every `exec(new, old)`,
with `program` the file defining the code that called `exec`, including `.c`
and without a leading slash or clone suffix (for example, `"secure/login.c"`).
An inherited function names its defining file; a simul_efun wrapper names
the simul_efun's file. An efun pointer names the file that created it, or `0`
when no defining program is available.

A non-zero return allows the handover; zero refuses it, and `exec` returns
`0` having changed nothing.

`exec` is refused when the master does not define `valid_exec`, so a master
that never defines it has no `exec` at all.

The apply runs inside the caller's task: `this_player` is whatever it was at
the call to `exec`, `this_object` is the master, and an error thrown here is
the caller's error.

### Examples

```c
int valid_exec(string program, object new, object old) {
    return program == "secure/login.c";
}
```

### See also

`exec`, `connect`
