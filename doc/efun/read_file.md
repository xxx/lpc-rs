# read_file

`string read_file(string path)`

The whole of the file at `path` as a string. `path` is an in-game path; a
relative one is resolved against the calling object's directory.

Every call is first put to the master's `valid_read(path, "read_file",
caller, program)`. A refusal, or a master that does not define the apply, is
the runtime error `read_file: permission denied`. A file that cannot be read
is a runtime error naming its in-game path. Both are caught by `catch()`;
`read_file` never returns 0 for a failure.

A path that leads out of the lib (`..` past the root) is a runtime error
before the master is asked.

The read is live: a `write_file` earlier in the same task has not landed yet
(writes land when the task commits), so `read_file` sees the file as it was.

### Examples

```c
string motd = read_file("/etc/motd");
if (catch(motd = read_file("/secure/passwd"))) write("Not for you.\n");
```

### See also

`write_file`, `get_dir`, `rm`, `valid_read`
