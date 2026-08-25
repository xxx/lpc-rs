# command

`int command(string line, [object living = this_player()])`

Runs `line` as if `living` had typed it, inside the calling task's transaction:
`process_input`, the living's rules, and the failure message all run here.
Returns 1 when the line was handled, 0 otherwise (including when `living` is
not a living object or `this_player()` is unset). Rules `init()` registered
earlier in the same task are already usable.

### Examples

```c
move_object("/area/start/spark");
command("look");
```

### See also

`add_action`, `process_input`, `notify_fail`
