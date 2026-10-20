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
Existence, file type and offset bounds use that same view after `valid_write`
returns, including earlier creation, growth, truncation, removal or renaming.
Negative offsets count from this pending end. The function does not create a
missing file, but a file created earlier in the task with `write_file` is present.
Pending renames use the source's disk contents, as described by [`rename`](rename.md).

### Examples

```c
write_bytes("/d/map.m", y * width + x, to_bytes("#", "UTF-8"));

write_file("/archive.bin", "", 1);
write_bytes("/archive.bin", 0, to_bytes(({0, 255})));
write_bytes("/archive.bin", file_size("/archive.bin"), to_bytes(({65})));
// The committed file contains bytes 0, 255, 65.
```

### See also

`read_bytes`, `to_bytes`, `write_chars`, `write_file`
