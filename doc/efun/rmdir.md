# rmdir

`int rmdir(string path)`

Remove the empty directory `path`, once the master's `valid_write` allows
it, and return `1`. A missing path, a path that is not a directory, a
directory with entries in it, or a refusal by the master is an error.
`path` is resolved against the calling object's directory.

The directory is removed when the task commits. A task that ends in an
error removes nothing.

### See also

`mkdir`, `rm`, `get_dir`, `file_size`
