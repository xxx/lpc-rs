# catch_tell

`void catch_tell(string message)`

The driver applies `catch_tell` in an object whenever there is in-game text to
send to it. This includes output from commands, and messages from other
players. Efuns like `write`, `say` and `tell_object` use this as their
communication mechanism.

Any object may define `catch_tell`, player or NPC, living or not. An object
without it receives the text on its connection when it has one; otherwise the
text goes to the debug log.

### See also

`write`, `say`, `tell_object`
