# read_bytes

`bytes | int read_bytes(string path, int start = 0, int length = to the end)`

Return `length` bytes of the file at `path` from byte `start`, once the
master's `valid_read` allows it. A negative `start` counts back from the end
of the file and stops at its beginning. A read that would run past the end
is cut at the end; with no `length`, the read runs to the end. When `start`
is at or past the end, the result is `0`, not `bytes`. A negative `length`
is an error.

The result is a `bytes` value; use `to_text` to decode it, or
`read_chars`/`read_file` for text. A range longer than 8192 bytes is an
error.

A missing or unreadable file is an error, as is the master's refusal.

### Examples

```c
bytes tail = read_bytes("/log/driver", -4000);   /* the last 4000 bytes */
bytes cell = read_bytes("/d/map.m", y * width + x, 1);
```

### See also

`write_bytes`, `to_text`, `read_chars`, `read_file`, `file_size`
