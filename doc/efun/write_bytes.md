# write_bytes

`int write_bytes(string path, int start, string str)`

Overwrite the file at `path` from byte `start` with the bytes of `str`,
once the master's `valid_write` allows it, and return 1. A negative `start`
counts back from the end of the file; a `start` equal to the file's size
appends. The result is 0, with nothing written, when the file does not
exist or `start` is past its end. The master's refusal is an error.

Like `write_file`, the write is checked now and lands when the task
commits: a `read_bytes` later in the same task sees the bytes as they were.

### Examples

```c
write_bytes("/d/map.m", y * width + x, "#");
```

### See also

`read_bytes`, `write_file`
