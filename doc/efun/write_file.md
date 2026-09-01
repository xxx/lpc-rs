# write_file

`int write_file(string path, string contents)`

Append `contents` to the file at `path`, creating the file when it does not
exist. Returns 1. `path` is an in-game path; a relative one is resolved
against the calling object's directory.

Every call is first put to the master's `valid_write(path, "write_file",
caller, program)`. A refusal, or a master that does not define the apply, is
the runtime error `write_file: permission denied`. The parent directory must
exist and `path` must not be a directory; either is a runtime error naming
the in-game path. All are caught by `catch()`.

The append itself happens when the task commits: a task that errors out
writes nothing, a task the driver re-runs writes once, and two `write_file`s
in one task land in order. A `read_file` of the same file later in the task
sees the contents from before the task. A failure at that point (the disk
is full, the directory was removed meanwhile) goes to the debug log; there
is no caller left to tell.

### Examples

```c
write_file("/log/access", name + " logged in\n");
```

### See also

`read_file`, `rm`, `valid_write`
