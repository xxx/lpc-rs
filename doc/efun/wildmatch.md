# wildmatch

`int wildmatch(string pattern, string subject)`

Whether the whole of `subject` matches the shell-style `pattern`: 1 or 0.
In the pattern, `*` matches any run of characters (an empty one
included), `?` matches any one character, `[set]` matches one character
in the set and `[^set]` one not in it, and `\c` matches `c` whatever it
is. Inside a set, `a-z` is an inclusive range; a `]` or `-` written first
is a member, as is a `-` written last. A pattern with an unclosed `[` or
a trailing `\` matches nothing. Characters are counted, not bytes.

A non-string `subject` answers 0, so `wildmatch("to *", str)` is safe when
`str` may be 0. A non-string `pattern` is an error.

### Examples

```c
wildmatch("*.c", "room.c");       // 1
wildmatch("[at][to] *", "to bob"); // 1
wildmatch("?", "");                // 0
```

### See also

`regexp`, `sscanf`, `explode`
