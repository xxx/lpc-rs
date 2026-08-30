# exec

`int exec(object new, object old)`

Move an interactive connection from one object to another. Once complete, the
object `new` will be the interactive object, and `old` will be non-interactive.
`this_player()` and `this_object()` do _not_ change.

If `new` has an existing interactive connection, it will be disconnected.

Returns `1` on success, `0` when `old` has no connection to move. Passing the
same object as both `new` and `old` is a runtime error.

This efun is very powerful, and can be used to take over the game. Every call
is first put to the master's `valid_exec(caller, new, old)`; a refusal, or a
master that does not define it, makes `exec` return `0`.

### See also:

`valid_exec`, `connect`, `this_player`, `set_this_player`
