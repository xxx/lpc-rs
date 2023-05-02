# shutdown

`void shutdown()`

The driver calls `shutdown` on the master object immediately prior to exiting
the process, during an orderly shutdown. This is where last minute saving
of player files or any other work should be done. This is the last place to run
LPC code.

At the point `shutdown` is applied, new connections have been disabled, 
players can no longer issue commands, and call outs will no longer run.

Players _can_, however, still receive output from the game.

Existing connections will be closed after `shutdown` returns.

The driver allows much longer for `shutdown` to complete than it does for
typical applies, due to the potential need to persist data, and how bad things
can go if that partially fails.

Defining `shutdown` is optional, and it will simply not be called if not defined.
