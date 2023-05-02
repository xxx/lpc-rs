# enable_commands

`void enable_commands()`

Turns `this_object` into a living object, which allows it to both
execute commands, and receive input from the environment (via `catch_tell`).

After calling `enable_commands`, `living()` will return `1` for this object.

### See also:

`disable_commands`, `living`