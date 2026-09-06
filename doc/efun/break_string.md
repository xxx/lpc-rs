# break_string

`string break_string(string str, int width [, int | string indent])`

`str` word-wrapped: lines of at most `width` characters, broken at the
last space that keeps a line within the width. A word longer than a line
stands on a line of its own, unbroken. A newline already in `str` ends a
line, and a trailing newline is kept. `indent`, a count of spaces or a
string, goes in front of every line and counts toward the width; a width
at or below the indent's length leaves one character of room. Characters
are counted, not bytes or display columns.

A non-string `str` answers 0, so the call is safe on a value that may be
0. A width below 1 is taken as 1. An indent that is neither an int in
0..1000 nor a string is an error.

### Examples

```c
break_string("ab cd ef gh ij kl mn op qr", 8);
// "ab cd ef\ngh ij kl\nmn op qr"
break_string("ab cd ef", 6, 2);
// "  ab\n  cd\n  ef"
write(break_string(text, 76) + "\n");
```

### See also

`sprintf`, `implode`, `explode`
