# dump

`void dump(mixed x, ...)`

Dump one or more variables to `this_player()`, as `write` does: through
`catch_tell`, else the player's connection, else the debug log. Variables will be
separated by a single space, à la Javascript's `console.log`. A `bytes` prints
as `b"..."`: printable ASCII as itself, `"` and `\` escaped, every other byte
as `\xNN`.

### See Also
`write`