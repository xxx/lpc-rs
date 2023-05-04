# error_handler

`void error_handler(mapping error)`

The driver calls `error_handler` on the master object when an error occurs
during compilation or at runtime. The master can then decide what to do with
it from there.

The error mapping has the following keys:

* `error["error"]` (string) - The error message.
* `error["location"]` (string) - The file:line:column where the error occurred.
* `error["object"]` (object) - The object in which the error occurred.
* `error["diagnostic"]` (string) - The full diagnostic message, if any.
  _note:_ This field may be broken up further in the future.

Diagnostic example:
```c
error: call to unknown function `clone_obect`
   ┌─ /home/mpd/git-sources/lpc-rs/lib/secure/master.c:15:12
   │
15 │     return clone_obect("/secure/login");
   │            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Defining `error_handler` is optional, and errors will instead be written to the
debug log if not defined.
