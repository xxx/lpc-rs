# parse_command_users

`object *parse_command_users()`

Applied in the master when a `parse_add_rule` rule has a `LIV`/`LVS` slot,
so it can offer livings beyond the actor's own scope — most usefully,
players elsewhere in the game. Every returned object that has called
`enable_commands()` becomes a candidate a `LIV`/`LVS` phrase may name,
reachable; it is never offered to an `OBJ`/`OBS` phrase in the same rule.
Anything else in the result is dropped; undefined, or a result that is not
an array, adds nobody.

Departures from MudOS are listed in [`parse_sentence`](../../efun/parse_sentence.md)'s
departures table.

### See also

[`parse_add_rule`](../../efun/parse_add_rule.md),
[`parse_sentence`](../../efun/parse_sentence.md), `enable_commands`
