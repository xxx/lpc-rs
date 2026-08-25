# remove_action

`int remove_action(string verb)`
`int remove_action(string function, string verb)`
`int remove_action(string verb, object living)`

Unregisters the calling object's rules for `verb`: from `this_player()`, or —
with a function name first — only the rules handled by that function, or —
with an object second — from that living. Returns how many rules were removed.

### See also

`add_action`
