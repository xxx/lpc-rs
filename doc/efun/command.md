# command

`int command(string line, [object living = this_object()])`

Runs `line` as if `living` had typed it, inside the calling task's transaction:
the master's `modify_command`, `process_input`, the living's rules, and the
failure message all run here.
If `living` is omitted or 0, it defaults to `this_object()`, even when
`this_player()` is unset or refers to another object. Pass `this_player()`
explicitly to run a command for the current command giver.

Returns 1 when the line was handled, 0 otherwise (including when `living` is
not a living object). Rules `init()` registered earlier in the same task
are already usable.

A handler may run `command()` of its own, nested at most 16 deep; deeper is a
runtime error.

### Examples

```c
move_object("/area/start/spark");
command("look");
```

### See also

`add_action`, `add_rule`, `modify_command`, `process_input`, `notify_fail`
