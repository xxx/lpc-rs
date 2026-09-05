# valid_shutdown

`int valid_shutdown(object caller, string program)`

The driver applies `valid_shutdown` in the master before every `shutdown`,
with `caller` the object whose code called it and `program` the file that
defines that code (a simul_efun wrapper hands the master the simul_efun
object and file). A non-zero return allows the shutdown; zero refuses it,
and `shutdown` throws `permission denied` having changed nothing.

`shutdown` is refused when the master does not define `valid_shutdown`, so a
master that never defines it has no `shutdown` at all.

The apply runs inside the caller's task: `this_player` is whatever it was at
the call to `shutdown`, `this_object` is the master, and an error thrown here
is the caller's error.

### Examples

```c
int valid_shutdown(object caller, string program) {
    // Only the shutdown daemon may stop the driver.
    return program == "/secure/armageddon.c";
}
```

### See also

`shutdown` (efun), `shutdown` (apply), `valid_exec`
