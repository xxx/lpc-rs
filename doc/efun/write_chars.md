# write_chars

`int write_chars(string path, int start, string str)`

`write_bytes`, measured in UTF-8 characters: replace the `strlen(str)` characters
of the file at `path` from character `start` with `str`, once the master's
`valid_write` allows it, and return 1. A negative `start` counts back from
the end of the file; a `start` equal to the file's character count appends;
a replacement that runs past the end replaces what is there and extends the
file with the rest. The result is 0, with nothing written, when the file
does not exist or `start` is past its end. The master's refusal is an
error.

The replaced characters and `str` need not take the same number of bytes:
a one-byte cell can become a three-byte one, and the rest of the file moves
to make room. The whole file must be UTF-8, or the call is an error naming
it.

Like `write_file`, the write is checked now and lands when the task
commits; a `read_chars` later in the same task already sees the new
characters, and two writes in one task land in order, each seeing the
other's characters.

### Examples

```c
write_chars("/d/map.m", y * width + x, "森");
```

### See also

`read_chars`, `write_bytes`, `write_file`
