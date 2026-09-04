# rename

`int rename(string from, string to)`

Move the file or directory `from` to `to`, once the master's `valid_write`
allows both paths, and return `0` on success: the C convention, inverted
from the other file efuns. When `to` is an existing directory, `from` is
moved into it under its own name; otherwise `to` is the new name, and a
file already there is replaced. A missing `from`, a `to` whose parent
directory does not exist, or a refusal by the master for either path is an
error. Both paths are resolved against the calling object's directory.

The move happens when the task commits; a read of either path later in the
same task sees the files as they were. A task that ends in an error moves
nothing.

### Examples

```c
rename("/players/wizard/obj.c", "/players/wizard/newobj.c");
rename("/players/wizard/obj.c", "/players/wizard/old");   // into the directory
```

### See also

`rm`, `mkdir`, `rmdir`, `write_file`
