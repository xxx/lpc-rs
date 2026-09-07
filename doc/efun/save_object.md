# save_object

`string save_object(string file)`

Write the calling object's global variables to the save file `file`, one
`name value` line each, in declaration order with inherited variables
first. `static` variables are not written; `private` ones are. A variable
holding a function pointer is left out. `file` is an in-game path resolved
against the lib root (not the caller's directory), and `.o` is always
appended: `save_object("/players/a/ann")` writes `/players/a/ann.o`,
`save_object("/x.o")` writes `/x.o.o`. Returns the in-game path written.

Every call is first put to the master's `valid_write(path, "save_object",
caller, program)` with `path` as resolved but before the suffix. A refusal,
or a master that does not define the apply, is the runtime error
`save_object: permission denied`. The parent directory must exist; a missing
one is a runtime error naming the suffixed path. Both are caught by
`catch()`.

The file's text is built when the efun runs, so it is one consistent view of
the object, and written when the task commits, through a temporary file
renamed over the target: a task that errors out writes nothing, a task the
driver re-runs writes once, and a reader never sees a half-written file. A
failure at that point (the disk is full, the directory was removed
meanwhile) goes to the debug log; the efun has already returned.

A global holding a float that is not finite (infinity or NaN) makes the
efun raise a runtime error instead of writing the file.

The format is described in [`doc/save-format.md`](../save-format.md).

### Examples

```c
void save_me() { save_object("/players/" + name[0..0] + "/" + name); }
```

### See also

`restore_object`, `save_map`, `restore_map`, `valid_write`
