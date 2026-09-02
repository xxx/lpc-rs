# error_handler

`void error_handler(mapping error)`

The driver calls `error_handler` on the master object when an error occurs
during compilation or at runtime. The master can then decide what to do with
it from there.

The error mapping has the following keys:

* `error["error"]` (string) - The error message.
* `error["location"]` (string) - The in-game file:line:column where the error
  occurred.
* `error["object"]` (object) - The object in which the error occurred.
* `error["diagnostic"]` (string) - The rendered diagnostic, source excerpt
  included, no terminal color codes.

Diagnostic example:
```c
error: call to unknown function `clone_obect`
   ┌─ /secure/master.c:15:12
   │
15 │     return clone_obect("/secure/login");
   │            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Defining `error_handler` is optional, and errors will instead be written to the
debug log if not defined. An error thrown by `error_handler` itself goes to the
debug log too, after the error it was handling.

A `call_out` or `input_to` callback has no caller to receive its error, so an
uncaught error there arrives here with the receiver as `error["object"]`. When
the receiver itself cannot be resolved (its load refused by `valid_load`, its
compile failed, the pointer's owner destructed) the object is the one that made
the pointer.

A compile that fails carries its warnings inside the error's diagnostic; a
compile that succeeds hands its warnings to `warning_handler` instead.

### See also

`warning_handler`
