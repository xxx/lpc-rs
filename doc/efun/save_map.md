# save_map

`void save_map(mapping m, string file)`

Write the mapping `m` to the save file `file`, one `key value` line per
entry in the mapping's order. Every key must be a string; any other key is
the runtime error `save_map: a key must be a string, not <type>`, raised
before the master is asked or anything is written. A string key may be any
text except one containing a space, tab, newline or carriage return; a key
with any of those is the runtime error `save_map: a key cannot contain
whitespace: "<key>"`, raised at the same point. The empty string is a
valid key: it writes as a line beginning with a space. `file` is an
in-game path resolved against the lib root, and `.o` is always appended.

Every call is first put to the master's `valid_write(path, "save_map",
caller, program)` with `path` as resolved but before the suffix. A refusal,
or a master that does not define the apply, is the runtime error
`save_map: permission denied`. The parent directory must exist. The file is
written when the task commits, through a temporary file renamed over the
target, as `save_object` does. A failure at that point (the disk is full,
the directory was removed meanwhile) goes to the debug log; the efun has
already returned.

The format is described in [`doc/save-format.md`](../save-format.md).

### Examples

```c
save_map(([ "owner": name, "items": items ]), "/data/chest_" + id);
```

### See also

`restore_map`, `save_object`, `restore_object`, `valid_write`
