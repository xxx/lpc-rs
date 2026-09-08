# to_text

`string to_text(bytes b, string encoding)`
`string to_text(int *values, string encoding)`
`string to_text(int *codepoints)`
`string to_text(string s)`

Build a string:

- A `bytes` is decoded through the named `encoding`. Without an encoding it
  is an error: bytes carry no encoding of their own.
- An int array with an encoding is taken as byte values and decoded; an
  element outside 0 to 255 is an error.
- An int array with no encoding is taken as Unicode code points; an element
  that is not a code point is an error.
- A string is returned as it is.

The encodings, matched without regard to case, are `UTF-8` (also `UTF8`)
and `ISO-8859-1` (also `ISO8859-1`, `LATIN1`, `LATIN-1`). Any other name is
an error. A byte sequence the encoding does not admit is an error, never a
replacement character, so a decode that returns is a decode that was right.

### Examples

```c
string notes = to_text(read_bytes("/old/notes"), "latin1");
string line = to_text(({ 104, 105 }), "UTF-8");   /* "hi" */
string euro = to_text(({ 0x20ac }));              /* "€" */
```

### See also

`to_bytes`, `bytesp`, `to_string`, `read_bytes`, `read_file`
