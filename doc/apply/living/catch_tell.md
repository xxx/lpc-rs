# catch_tell

`void catch_tell(string message)`

The driver applies `catch_tell` in living objects whenever there is in-game
text to send to them. This includes output from commands, and messages from
other players. Efuns like `write`, `say` and `tell_object` use this as their
communication mechanism.

Note that `catch_tell` is applied on any living object, player or NPC, where
`enable_commands` has been called.

### See also

`enable_commands`, `write`, `say`, `tell_object`

