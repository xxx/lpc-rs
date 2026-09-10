# epilog

`string *epilog(int load_empty)`

The driver applies `epilog` on the master once the master is initialized
and before it listens for connections. The array it returns is the preload
list: the driver then applies `preload(file)` for each string in it, in
order, one task per file, and a file whose preload fails does not stop the
rest. An answer of 0, or no `epilog` at all, preloads nothing; any other
answer that is not an array preloads nothing and is noted in the debug log
with its type. An error thrown by `epilog` goes to `error_handler` and
boot continues without preloads. An entry that is not a string is
skipped, with a line in the debug log naming its type.

`load_empty` is always 0; lpc-rs has no "load empty" startup option.

`epilog` runs before the main loop: `this_player()` and `previous_object()`
are 0 and no connection exists yet. A `call_out` it schedules fires once
the main loop starts, after every preload, which is where work the lib
wants done "once the game is up" belongs.

`epilog` and each `preload` run under the configured `max_execution_time`.

### Examples

```c
string *epilog(int load_empty) {
    call_out(&final_boot(), 0);
    return ({ "/d/Gondor/start", "/obj/board" });
}
```

### See also

`preload`, `shutdown`, `error_handler`, `call_out`
