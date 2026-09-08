# write_bytes

`int write_bytes(string path, int start, bytes b)`

Overwrite the file at `path` from byte `start` with `b`, once the master's
`valid_write` allows it, and return 1. A negative `start` counts back from
the end of the file; a `start` equal to the file's size appends. The
result is 0, with nothing written, when the file does not exist or `start`
is past its end. The master's refusal is an error.

A string is refused; encode it with `to_bytes`.

Like `write_file`, the write is checked now and lands when the task
commits; a `read_bytes` later in the same task already sees the new bytes.

### Examples

```c
write_bytes("/d/map.m", y * width + x, to_bytes("#", "UTF-8"));
```

### See also

`read_bytes`, `to_bytes`, `write_chars`, `write_file`
