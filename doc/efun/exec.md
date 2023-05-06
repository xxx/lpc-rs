# exec

`int exec(object new, object old)`

Move an interactive connection from one object to another. Once complete, the
object `new` will be the interactive object, and `old` will be non-interactive.
`this_player()` and `this_object()` do _not_ change.

If `old` has an existing interactive connection, it will be disconnected.

Returns `1` on success, `0` on failure.

This efun is very powerful, and can be used to take over the game. Its use
should be severely restricted.

TODO: add security instructions once implemented

### See also:

`connect`, `this_player`, `set_this_player`
