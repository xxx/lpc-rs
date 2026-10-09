# rmdir

`int rmdir(string path)`

Remove the empty directory `path`, once the master's `valid_write` allows
it, and return `1`. A missing path, a path that is not a directory, a
directory with entries in it, or a refusal by the master is an error.
`path` is resolved against the calling object's directory.

The directory is removed when the task commits; a `file_size` of it later
in the same task already answers -1. A task that ends in an error removes
nothing.

Both existence and emptiness account for earlier filesystem changes in the
same task. Removing the last child allows removal of its parent, including
directories created earlier in the task. Pending child files, saves and
directories prevent removal. Renaming the directory itself carries its contents;
moving children updates membership. Removing the same directory twice raises
a missing-path error on the second call.

### See also

`mkdir`, `rm`, `rename`, `get_dir`, `file_size`
