# previous_object

`object | object *previous_object([int step = 0])`

Returns the object that called the current function through a door: `->`
(a call to `this_object()` included, and each element of an array or
mapping receiver), a function-pointer call, a simul efun, or the start of
a task. A local call, own or inherited, is not a door: inside it
`previous_object()` answers what it answered in the caller.

At the start of a task it is the object whose code started the task:
`create()` sees the object that cloned, found or called it, a master apply
sees the object whose efun asked, `catch_tell` sees the object that wrote
or told, and a virtual object's `create()` sees the original requester. A
`call_out` or `input_to` callback sees the pointer's owner, a command
handler sees the actor, and a task the driver starts on its own (boot,
login, a connection event) sees 0. A destructed previous object is 0.

`step` counts back: `previous_object(1)` is what `previous_object()`
answered in the previous object, and so on across task starts; past the
end of the chain the answer is 0. `previous_object(-1)` is the whole chain
as an array, innermost first, a destructed member 0 in its place. Any
other negative step is a runtime error.

### Examples

```c
// In /d/room.c, reached by "/d/room"->enter() from /obj/player#3:
void enter() {
    previous_object();      // /obj/player#3
    check();                // a local call
}
void check() {
    previous_object();      // still /obj/player#3
    this_object()->probe(); // -> to self is a door
}
void probe() {
    previous_object();      // /d/room
    previous_object(1);     // /obj/player#3
    previous_object(-1);    // ({ /d/room, /obj/player#3, ... })
}
```

### See Also

`this_object`, `call_other`, `call_out`, `add_action`
