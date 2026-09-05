# shutdown

`void shutdown(int code = 0)`

Stop the driver once the calling task commits, with process exit code
`code`. The master's `valid_shutdown(caller, program)` must allow it; a
refusal, or a master without that apply, is a `permission denied` error and
nothing happens. A `code` outside the C `int` range is an error.

The request lands with the task's other effects: a task that errors after
calling `shutdown` stops nothing. When it lands, the driver leaves its main
loop, tells every connection, runs the master's `shutdown()` apply, closes
every connection, and exits with `code`.

FluffOS's convention is that a restart script treats `-1` as "stay down";
a reboot command calls `shutdown()` and a halt command `shutdown(-1)`.

### Examples

```c
/* /secure/armageddon.c, allowed by the master's valid_shutdown */
void go_down() { shutdown(0); }
```

### See also

`valid_shutdown` (apply), `shutdown` (apply), `exec`
