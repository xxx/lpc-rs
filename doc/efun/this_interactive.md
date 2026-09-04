# this_interactive

`object this_interactive()`

Return the interactive object whose command started the current task: the
command giver the task began with, as long as it still has a connection.
Unlike `this_player`, it is not moved by `set_this_player`, so code that
runs as an NPC can still find the person who typed the command. Returns
`0` when the task started without a command giver, or when that object has
no connection, as an NPC has not.

### Examples

```c
void do_look() {
    // The player, even after set_this_player(npc).
    object who = this_interactive();
}
```

### See also

`this_player`, `set_this_player`, `interactive`, `users`
