# file_size

`int file_size(string path)`

Return the size in bytes of the file at `path`, `-1` when there is no such
file, or `-2` when `path` is a directory. The master's `valid_read` is
asked first, and a refusal answers `-1` too, so the call is a safe probe:
`file_size(path) < 0` means there is nothing there to read. `path` is
resolved against the calling object's directory. A non-string `path`, or
one that leaves the lib, is an error.

### Examples

```c
if (file_size(path) == -2) {
    // a directory
}
```

### See also

`get_dir`, `read_file`, `write_file`, `rm`, `mkdir`
