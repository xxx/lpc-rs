# this_player

`object this_player()`

Returns the current command giver, which may be an NPC or an interactive player.
`set_this_player` can change it during execution. A call-out starts without a
player by default; with `LPC_CALL_OUT_PRESERVE_PLAYER=true`, it starts with the
player captured when scheduled. See [`call_out`](call_out.md) for lifecycle and
retry behavior.

### See Also

`set_this_player`, `this_interactive`, `call_out`
