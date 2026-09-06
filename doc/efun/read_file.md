# read_file

`string read_file(string path, int start = 1, int lines = 0)`

The file at `path` as a string, or the lines `start` and `lines` select.
`path` is an in-game path; a relative one is resolved against the calling
object's directory.

`start` is a 1-based line number (0 is the first line too) and `lines` a
count; `0` or an absent `lines` reads to the end. A start past the last line
answers `""`. A negative `start` or `lines` is a runtime error. Lines keep the
newline they had in the file.

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
string tail = read_file("/log/runtime", 100, 20);
```

### See also

`write_file`, `get_dir`, `rm`, `valid_read`
