# get_dir

`string *get_dir(string path)`

The names of the entries in the directory at `path`, sorted, as plain names
(no directory prefix, no type markers); `({ })` for an empty directory. A
`path` that is not a directory is a runtime error naming its in-game path.

Every call is first put to the master's `valid_read(path, "get_dir", caller,
program)`. A refusal, or a master that does not define the apply, is the
runtime error `get_dir: permission denied`. Both errors are caught by
`catch()`.

No wildcards: `get_dir("/players/*.o")` is an error, not a filter.

### Examples

```c
string *names = get_dir("/players");
```

### See also

`read_file`, `write_file`, `rm`, `valid_read`
