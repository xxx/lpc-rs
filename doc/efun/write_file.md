# write_file

`int write_file(string path, string contents, int mode = 0)`

Write `contents` to the file at `path`, creating the file when it does not
exist. Returns 1 after staging the write. `path` is an in-game path; a relative
one is resolved against the calling object's directory.

| Mode | Behavior |
| --- | --- |
| `0` or omitted | Append to the existing contents. |
| `1` | Replace the entire file, including when `contents` is empty. |

An explicitly supplied mode must be an integer `0` or `1`; other types and
values raise a runtime error. `contents` must be a string and is written as
UTF-8, including embedded NULs.

Every well-formed call is put to the master's `valid_write(path, "write_file",
caller, program)`. A refusal, or a master that does not define the apply, is
the runtime error `write_file: permission denied`. The parent directory must
exist and `path` must not be a directory; either is a runtime error naming
the in-game path. All are caught by `catch()`.

Parent checks include directories created, removed, or moved earlier in the
same task.

The write itself happens when the task commits: a task that errors out
writes nothing, a task the driver re-runs writes once, and two `write_file`s
in one task land in order. A `read_file`, `file_size` or `read_chars` of
the same file later in the task already sees the pending contents; other
tasks see the file as it is until the commit. A failure at that point (the disk
is full, the directory was removed meanwhile) goes to the debug log; there
is no caller left to tell.

Overwrite mode writes a temporary sibling and renames it over the target;
it does not delete the original first. A caught permission refusal records
no file change, and a failed temporary write or rename leaves the target
untouched. Replacement creates a new filesystem entry with the temporary
file's permissions; an existing symbolic link to a file is replaced rather than written
through. Successful staging is not a guarantee of delivery or durability.

### Examples

```c
write_file("/log/access", name + " logged in\n");
write_file("/data/draft.txt", draft, 1);
```

### See also

`read_file`, `rm`, `valid_write`
