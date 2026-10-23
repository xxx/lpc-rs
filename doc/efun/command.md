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

The living's native and `add_action` rules run before parser-package rules.
That parser tier tries the longest registered verb phrase first, with
registration order within each phrase and shorter-phrase fallback. See
[`parse_sentence`](parse_sentence.md) for its selection and error policies.

A handler may run `command()` of its own, nested at most 16 deep; deeper is a
runtime error.

### Callback context

Callbacks see `living` as `this_player()` and immediate `previous_object()`,
with `origin()` equal to `"driver"`. This includes vocabulary used by native
`add_rule` patterns and parser-package rules: object ids, adjectives, plurals,
`id()` fallback, and the master's shared lists, numerals and pluralization.
Permissions and actions use the same actor context even when the entering
`this_player()` is another object or null.

Selecting the actor does not change the task's interactive provenance:
`this_interactive()` continues to follow its existing entry-player and
connection rules. Each callback starts with the actor as player; a callback's
`set_this_player()` affects its nested work without changing later callbacks.
Nested commands select their own actors and restore the surrounding player and
command frame on return or exception. Caught callback effects still follow
the current transaction's rules.

### Examples

```c
move_object("/area/start/spark");
command("look");
```

### See also

`add_action`, `add_rule`, `modify_command`, `process_input`, `notify_fail`
