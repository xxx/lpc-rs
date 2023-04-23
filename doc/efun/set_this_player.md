# set_this_player

`object set_this_player(object new_player)`

Allows setting a new value to be returned by `this_player()`. This is occasionally
useful, but typically indicates a code smell. The driver will automatically set
`this_player()` to the current user / NPC when a command is entered.

You can pass `0` to `set_this_player()` to clear the current value without setting
a new one.

This function returns the previous value of `this_player()`.

### See Also

`this_player()`