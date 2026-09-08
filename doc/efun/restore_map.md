# restore_map

`mapping restore_map(string file)`

Read the save file `file` as a mapping: each `name value` line becomes one
entry, in file order. `file` is an in-game path resolved against the lib
root, and `.o` is always appended. A file written by `save_object` reads
the same way, as a mapping of that object's variables.

A file that does not exist or cannot be read is an empty mapping, without
an error. A malformed line is the runtime error
`restore_map: <path> line <n>: <what>`, caught by `catch()`.

Every call is first put to the master's `valid_read(path, "restore_map",
caller, program)` with `path` as resolved but before the suffix. A refusal,
or a master that does not define the apply, is the runtime error
`restore_map: permission denied`.

A file that is not UTF-8 is read as Latin-1. The format is described in
[`doc/save-format.md`](../save-format.md).

### Examples

```c
mapping data;
if (catch(data = restore_map(filename))) data = ([]);
```

### See also

`save_map`, `save_object`, `restore_object`, `valid_read`
