# calling_object

`object | object *calling_object([int step = 0])`

Returns the object of the function that called the current function.
Every call counts, including local and inherited calls, so the result can
be `this_object()`. `previous_object()` counts external calls and task
entries; it does not advance for a local call.

`step` counts back through calling frames: `0` is the immediate caller,
`1` its caller, and so on. Past the end of the stack the answer is 0.
`calling_object(-1)` returns all callers as an array, innermost first,
including repeated objects. Any other negative step is a runtime error.
A destructed caller is 0 in its original position.

Synchronous nested tasks retain the frames that started them: a clone's
`create()` sees the function that called `clone_object`, followed by its
callers, including local calls. Driver-started calls have no LPC calling
function; where the driver supplies an object, such as a `call_out`'s
pointer owner or a command's actor, that object occupies the corresponding
position. A boot entry has no caller.

An efun pointer such as `&calling_object()` sees the frame that fired it.

### Examples

```c
// In /room.c, reached by "/room"->enter() from /player:
void enter() {
    calling_object();       // /player
    check();
}
void check() {
    calling_object();       // /room, also this_object()
    calling_object(1);      // /player
    previous_object();      // /player
    calling_object(-1);     // ({ /room, /player, ... })
}
```

### See also

`calling_function`, `calling_program`, `previous_object`, `this_object`
