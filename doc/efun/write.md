# write

`int write(mixed)`

Write a string to this_player() (which might actually be an NPC).

This function is a wrapper around `this_player()->catch_tell()`, and will 
return 0 or 1, depending on whether an object was able to receive the message. 
Both players and NPCs can receive messages via `write()`.

If `catch_tell()` isn't defined, the message goes straight to the player's
connection. If there is no current player, or the player has neither
`catch_tell()` nor a connection, the message is written to the debug log and
0 is returned.

While the name may be similar, this function is not the same as `write_socket()`,
which only works when called from within a player object, and cannot be interacted
with by NPCs.

### See Also

`write_socket`, `this_player`, `dump`