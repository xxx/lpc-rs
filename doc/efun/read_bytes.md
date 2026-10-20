# read_bytes

`bytes read_bytes(string path, int start = 0, int length = to the end)`

Return `length` bytes of the file at `path` from byte `start`, once the
master's `valid_read` allows it. A negative `start` counts back from the end
of the file and stops at its beginning. A read that would run past the end
is cut at the end; with no `length`, the read runs to the end. When `start`
is at or past the end, the result is the [configured null value](../lpc/null.md):
`nil` in distinct mode or `0` in zero mode. A zero `length` returns empty bytes
when `start` is before the end, and null at or past the end. A negative `length`
is an error.

The result is a `bytes` value; use `to_text` to decode it, or
`read_chars`/`read_file` for text. A read is of any length.

A missing or unreadable file is an error, as is the master's refusal.

Nonnegative reads of an unchanged regular file read only the requested range,
or the remaining tail when `length` is omitted. Negative starts, zero-length
requests, nonseekable inputs and pending changes can require reading the whole
file. Reads include the calling task's earlier file changes and any changes made
by `valid_read`. Pending renames use the source's disk contents, as described by
[`rename`](rename.md).

### Examples

```c
bytes tail = read_bytes("/log/driver", -4000);   /* the last 4000 bytes */
bytes cell = read_bytes("/d/map.m", y * width + x, 1);
```

### See also

`write_bytes`, `to_text`, `read_chars`, `read_file`, `file_size`
