# to_bytes

`bytes to_bytes(string text, string encoding)`
`bytes to_bytes(int *values)`
`bytes to_bytes(int *codepoints, string encoding)`
`bytes to_bytes(bytes b)`

Build a `bytes` value:

- A string is encoded, one character at a time, in the named `encoding`.
  Without an encoding it is an error: a string is characters, and only an
  encoding says which bytes those are.
- An int array with no encoding is taken as the byte values themselves; an
  element outside 0 to 255 is an error.
- An int array with an encoding is taken as Unicode code points, which are
  then encoded; an element that is not a code point is an error.
- A `bytes` is returned as it is.

The encodings, matched without regard to case, are `UTF-8` (also `UTF8`)
and `ISO-8859-1` (also `ISO8859-1`, `LATIN1`, `LATIN-1`). Any other name is
an error. A character that the encoding cannot hold — anything above code
point 255 in ISO-8859-1 — is an error, never a substitute character.

### Examples

```c
bytes utf8 = to_bytes("héllo", "UTF-8");     /* six bytes */
bytes latin = to_bytes("héllo", "latin1");   /* five bytes */
bytes raw = to_bytes(({ 0x1b, '[', '0', 'm' }));
write_bytes("/d/map.m", y * width + x, to_bytes("#", "UTF-8"));
```

### See also

`to_text`, `bytesp`, `to_string`, `read_bytes`, `write_bytes`
