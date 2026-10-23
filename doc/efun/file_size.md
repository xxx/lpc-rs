# file_size

`int file_size(string path)`

Return the size in bytes of the file at `path`, `-1` when there is no such
file, or `-2` when `path` is a directory. The master's `valid_read` is
asked first, and a refusal answers `-1` too. `path` is
resolved against the calling object's directory. A non-string `path`, or
one that leaves the lib, is an error. A write, removal or `mkdir` of the
path earlier in the same task is already measured, though it lands on disk
only when the task commits.

This reads filesystem metadata and applies pending text replacements, appends,
byte writes, removals and directory creation without opening or reading the
contents. Pending character replacements or renames use the task's content view
and can require reading contents. Other filesystem errors are runtime errors
naming the in-game path. A nonnegative size does not guarantee that a later
content read will be permitted or succeed.

### Examples

```c
if (file_size(path) == -2) {
    // a directory
}
```

### See also

`file_time`, `get_dir`, `read_file`, `write_file`, `rm`, `mkdir`
