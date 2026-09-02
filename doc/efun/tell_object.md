# tell_object

`int tell_object(object | string ob, string message)`

Write a string to `ob` (which might actually be an NPC).

This function is a wrapper around `ob->catch_tell()`, and will 
return 0 or 1, depending on whether the object was able to receive the message. 
Both players and NPCs can receive messages via `tell_object`.

`ob` can be passed as a file_path, in which case the object will be loaded
(through the master's `valid_load`) and the message sent to it. If the
object cannot be loaded, the error propagates. A path with no source file
is put to the master's `compile_object`.

If `catch_tell` isn't defined in `ob`, the message goes straight to its
connection. If there is no object, or it has neither `catch_tell` nor a
connection, the message is written to the debug log and 0 is returned.

### See Also

`write`, `catch_tell`