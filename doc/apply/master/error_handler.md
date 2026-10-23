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
* `error["frames"]` (array of mappings, when available) - A bounded snapshot
  of active LPC frames at the uncaught failure, ordered from root to leaf.
* `error["frames_omitted"]` (int, when frames are available) - The number of
  outer frames excluded by snapshot limits.

Each frame mapping contains:

| Key | Value |
|---|---|
| `function` | Function name, including generated names for anonymous closures. |
| `program` | The compiled function's defining program path. |
| `object` | The receiver's object name as a string, including any clone suffix. |
| `file`, `line`, `column` | The current instruction's source file and one-based position; an empty file and zero position mean unknown. The file can differ from the program through inclusion. |
| `arguments`, `locals` | Arrays of variable mappings in declaration order. Arguments are fixed parameters; a named variadic collector appears among locals. Implicit closure arguments use names such as `$1`. |
| `variables_omitted` | Number of argument/local slots excluded by the limits. |
| `truncated` | Integer 1 if a field was shortened or variable slots omitted, otherwise 0. |

A variable mapping contains `name`, `type`, `value` and `truncated`.
The type is the runtime type name, or `"unknown"` when the slot cannot be read.
The value is rendered text: numbers and nil use their ordinary spelling,
strings are quoted with control characters escaped, and arrays, mappings and
bytes show their sizes. Classes show their nominal identity, objects their names
or a destructed marker, and functions a summary. An unreadable slot uses
`"<unavailable>"`. `truncated` is integer 1 when the name or value was shortened.
These strings are diagnostic data, not values to execute or parse back into LPC.

Parameters show their current values, including writes through `ref` or closure
cells. Locals include retained named slots, even when their lexical block has
finished or their declaration has not executed. Shadowed declarations can have
the same name. Compiler temporaries and captured variables belonging only to an
outer function are not added to the local list. The snapshot freezes rendered
data before rollback and contains no live container graphs or object references;
later changes or garbage collection do not change it. Source text itself is not
saved, so reading the file later may show a newer version.

The limits are 32 frames, 32 variables per frame, 256 UTF-8 bytes per rendered
field and 65,536 rendered bytes across all frames. Strings shortened by the byte
limit end in `...` at a UTF-8 boundary. Frames nearest the failure are retained,
and variables are considered with arguments first. Snapshot order stays
root-to-leaf among the retained frames. The limits apply across nested tasks too.

Frame capture occurs only on uncaught evaluation errors or interrupted active
evaluation. A direct compilation failure, an error before a frame is entered,
or a timeout between attempts has no snapshot and omits both frame keys.
An LPC-triggered compilation failure can retain the active caller frames.
[`catch`](../../efun/catch.md) keeps its string result, does not invoke this
handler and provides no saved-frame interface.

To find the player to show the error to, call `this_interactive()` inside the
handler. For a command, `input_to`, or `get_char` failure, it returns the
connected player whose input triggered the operation, including when a room or NPC callback
failed. The failed operation's `set_this_player()` calls do not change this
recipient. `error["object"]` and `previous_object()` identify the object being
reported and may be a different object.

```c
void error_handler(mapping error) {
    object player = this_interactive();
    if (player) {
        tell_object(player, error["diagnostic"]);
    }
}
```

The mudlib can check the player's wizard level here to choose between the full
diagnostic and a generic error message. The driver always logs the full error.

A `call_out` or boot apply has no initiating player: `this_interactive()`
returns `0`, even if a player originally scheduled the call out. For those
errors, the mudlib can use `error["object"]` to find an object's creator or
other recipients according to its own rules, or rely on the server log.

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
debug log and directly to the command giver's connection, when one exists.
If the handler throws or times out, both errors go to the server and debug
logs, and the original diagnostic goes to that connection; the driver does
not call the handler recursively. A successful handler controls player output.
The handler runs in a fresh transaction, so its output survives the failed
operation's rollback. Its mapping and nested frame data are built again within
each handler attempt on retry. Legacy `runtime_error(...)` applies are not called.

An exhausted execution limit is a runtime error too. Its diagnostic includes
the interrupted LPC source line and nested call stack when evaluation was
in progress, plus attempt and conflict counts. A timeout during retry backoff
has no active LPC frame. Conflicts indicate concurrent changes invalidated
earlier attempts; zero conflicts means the operation exhausted its allowance
without a rejected commit. Uncommitted state, output, and scheduled call outs
are discarded, and the connection can accept another command.

To trace apply calls, set `RUST_LOG=info,lpc_rs::applies=debug` and restart
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

`warning_handler`, `previous_object`, `this_interactive`, `tell_object`
