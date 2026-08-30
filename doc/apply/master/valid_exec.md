# valid_exec

`int valid_exec(object caller, object new, object old)`

The driver applies `valid_exec` in the master before every `exec(new, old)`,
with `caller` the object whose code called `exec`. A non-zero return allows
the handover; zero refuses it, and `exec` returns `0` having changed nothing.

`exec` is refused when the master does not define `valid_exec`, so a master
that never defines it has no `exec` at all.

The apply runs inside the caller's task: `this_player` is whatever it was at
the call to `exec`, `this_object` is the master, and an error thrown here is
the caller's error.

### Examples

```c
int valid_exec(object caller, object new, object old) {
    // Only the login object may hand a connection to a body.
    return file_name(caller) == "/secure/login";
}
```

### See also

`exec`, `connect`
