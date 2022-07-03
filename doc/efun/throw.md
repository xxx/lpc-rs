# throw

`void throw(mixed x)`

Not an efun per se, but immediately throws a runtime error, with the message being
whatever `x` stringifies to. If a `catch` is active, the error will be caught and
execution will continue.
