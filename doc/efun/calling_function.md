# calling_function

`string | string *calling_function([int step = 0])`

The name of the function that called the current one. Every call counts,
including local and inherited calls, function pointers, and simul efuns.
Synchronous nested tasks retain their calling frames: a clone's `create()`
sees the function that called `clone_object`, followed by that function's
callers.

Where the driver fired a call without an LPC function — a `call_out` or
`input_to` callback, a command handler, or boot — the answer is 0.
An efun pointer's internal frame also has no LPC function. A destructed
caller still names its function, where `calling_object()` answers 0.

`step` selects the same frame as `calling_object(step)`: `0` is the
immediate caller, `1` its caller, and so on. Past the end the answer is 0.
`calling_function(-1)` returns all callers' function names as an array,
innermost first, with 0 in an unknown position. Any other negative step
is an error.

### Examples

```c
// In /x.c, reached by "/x"->probe() from open_soul() in /player.c:
void probe() {
    calling_function();      // "open_soul"
    previous_object();       // /player
    check();                 // a local call
}
void check() {
    calling_function();      // "probe"
    calling_function(1);     // "open_soul"
}
```

### See also

`calling_object`, `calling_program`, `previous_object`, `call_other`
