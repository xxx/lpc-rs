# living

`int living(object ob = this_object())`

Returns `1` if `ob` is an living object, `0` otherwise.

A living object is one where `enable_commands` has been called. Living objects
can execute commands, and receive input from the environment (via `catch_tell`).

NPCs are living, as are players.

Being living is not the same as `interactive`, which is specific to objects
inhabited by a real player.

### See also:

`interactive`, `enable_commands`, `catch_tell`