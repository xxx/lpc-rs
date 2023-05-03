# write

`int tell_object(object | string ob, string message)`

Write a string to `ob` (which might actually be an NPC).

This function is a wrapper around `ob->catch_tell()`, and will 
return 0 or 1, depending on whether the object was able to receive the message. 
Both players and NPCs can receive messages via `tell_object`.

Only living objects (i.e. those where `enable_commands` have been called) will
receive messages via `tell_object`. If `ob` is not a living object, the message
will be written to the debug log.

`ob` can be passed as a file_path, in which case the object will be loaded
and the message sent to it. If the object cannot be loaded, the message will
be written to the debug log.

If there is no object, or `catch_tell` isn't defined in it, the message will
be written to the debug log.

### See Also

`write`, `catch_tell`, `enable_commands`