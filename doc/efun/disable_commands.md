# disable_commands

`void disable_commands()`

Turns `this_object` into a non-living object, preventing both entering commands,
and receiving input from the environment (via `catch_tell`).

Note that this does _not_ prevent a player entering input as a response to
an `input_to`, nor from receiving output sent directly via write_socket(). 
This mostly comes up during the login flow.

After calling `disable_commands`, `living()` will return `0` for this object.

### See also:

`enable_commands`, `living`