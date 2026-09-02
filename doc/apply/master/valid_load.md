# valid_load

`int valid_load(string path, string func, object caller, string program)`

The driver applies `valid_load` in the master before it compiles a file into
an object on LPC's behalf: `clone_object` of a prototype that is not
resident, and a string receiver — `"/x"->f()`, a `&->f()` pointer whose
receiver argument is a path, `move_object`, `tell_object`, `find_object` —
that names an object not yet loaded. A non-zero return allows the compile;
zero refuses it, and the efun raises `<func>: permission denied` in the
caller (`find_object` returns 0). A path that leads out of the lib fails
before this apply. An object that is already resident is found, not loaded,
and nothing is asked; cloning a resident prototype is not a load.

- `path` is the source file about to be compiled: the object's canonical
  in-game name with `.c` appended (`clone_object("/std/sword")` and
  `clone_object("/std/sword.c")` both ask about `"/std/sword.c"`).
- `func` is the efun's name, or `"call_other"` for `->` and for a `&->f()`
  pointer.
- `caller` is the object whose code asked for the load; for a pointer, the
  object that wrote it.
- `program` is the in-game path of the file that defines the calling code,
  extension included, as in `valid_read`: an inherited function names the
  parent's file, a closure or pointer the file it was written in; 0 for an
  efun pointer fired straight from `call_out`.

The load is refused when the master does not define `valid_load`, so a
master that never defines it loads nothing after boot — including its own
preload: the master is resident before its `create()` runs, so its loads pass
through here with `program` `"/secure/master.c"`. The simul_efun object's
`create()` runs before the master exists; a load from there is refused.

Once compiled, the new program's own `inherit`s and `#include`s are put to
`valid_inherit` and `valid_read` for the file being compiled, not for the
loader: the loader's permission ends at the load.

The apply runs inside the loading task: `this_object` is the master, and an
error thrown here is the caller's error. For a `call_out` pointer it runs in
the attempt that resolves the receiver, before the call-out's own task.

### Examples

```c
// Anyone loads /std and their own directory; code under /secure loads anything.
int valid_load(string path, string func, object caller, string program) {
    if (!program) return 0;
    if (program[0..7] == "/secure/") return 1;
    if (path[0..4] == "/std/") return 1;
    return creator(path) == creator(program);
}
```

### See also

`valid_inherit`, `valid_read`, `clone_object`, `call_other`
