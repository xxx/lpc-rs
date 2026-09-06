# calling_function

`string | string *calling_function([int step = 0])`

The name of the function that called the current one through a door — the
function `previous_object()` was executing when it called: a `->`, a
function-pointer call, a simul-efun call, or the code that started this
task (`create()` of a clone sees the function that called `clone_object`).
A local call, own or inherited, is not a door: inside one
`calling_function()` answers what it answered in the caller. Where the
driver fired the call — a `call_out` or `input_to` callback, a command
handler, an apply, boot — the answer is 0 although `previous_object()`
still names the object.

`step` counts back exactly as `previous_object(step)` does:
`calling_function(1)` is the function `previous_object(1)` called from,
past the end of the chain the answer is 0, and `calling_function(-1)` is
the whole chain as an array, innermost first, with 0 in an unknown
position. Any other negative step is an error.

### Examples

```c
// In /x.c, reached by "/x"->probe() from open_soul() in /player.c:
void probe() {
    calling_function();      // "open_soul"
    previous_object();       // /player
    check();                 // a local call
}
void check() {
    calling_function();      // still "open_soul"
}
```

### See also

`calling_program`, `previous_object`, `call_other`
