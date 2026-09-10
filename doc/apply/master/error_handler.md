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

Uncaught errors always go to the server log, including their source diagnostic
and LPC stack trace when available, even when `error_handler` handles them.
Defining `error_handler` is optional. Without it, the error also goes to the
debug log. If the handler throws, both errors go to the server and debug logs;
the driver does not call the handler recursively.

For porting diagnostics, set `RUST_LOG=info,lpc_rs::applies=debug` and restart
the driver. This includes missing optional applies, argument types, return
types, integer results, and security decisions. Apply failures and missing
security or login applies are logged at the default `info` setting. Argument
values and string results are omitted from apply tracing. These messages go
to `SERVER_LOG_FILE` (or `LPC_SERVER_LOG_FILE`), independently of `DEBUG_LOG_FILE`.
Debug tracing is available in default debug and release builds; builds that
explicitly select `release_max_level_info` or a lower level omit it in release.

A `call_out` or `input_to` callback has no caller to receive its error, so an
uncaught error there arrives here with the receiver as `error["object"]`. When
the receiver itself cannot be resolved (its load refused by `valid_load`, its
compile failed, the pointer's owner destructed) the object is the one that made
the pointer.

A boot apply — `epilog`, or a `preload` — has no caller either; its error
arrives with the master as `error["object"]`.

A compile that fails carries its warnings inside the error's diagnostic; a
compile that succeeds hands its warnings to `warning_handler` instead.

In `error_handler`, `previous_object()` is `error["object"]`, with nothing
behind it; 0 when there is no object.

### See also

`warning_handler`, `previous_object`
