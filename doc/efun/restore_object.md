# restore_object

`int restore_object(string file)`

Read the save file `file` into the calling object's global variables.
`file` is an in-game path resolved against the lib root (not the caller's
directory), and `.o` is always appended. Each `name value` line sets the
global of that name; a name the object does not have, or one that is
`static`, is ignored, and a variable not named in the file keeps its
value. There is no type check: a string in the file lands in an `int`
variable as a string.

Returns 1 on success and 0, without an error, when the file does not exist
or cannot be read, which is how a lib tells a new player from a returning
one. A malformed line is the runtime error
`restore_object: <path> line <n>: <what>`; the lines before it have
already been applied, so a `catch()` sees a partly restored object, while
an uncaught error aborts the task and leaves the object as it was.

Every call is first put to the master's `valid_read(path,
"restore_object", caller, program)` with `path` as resolved but before the
suffix. A refusal, or a master that does not define the apply, is the
runtime error `restore_object: permission denied`.

A file that is not UTF-8 is read as Latin-1. The format is described in
[`doc/save-format.md`](../save-format.md).

### Examples

```c
if (!restore_object("/players/" + name[0..0] + "/" + name)) {
    write("New player.\n");
}
```

### See also

`save_object`, `save_map`, `restore_map`, `valid_read`
