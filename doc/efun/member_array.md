# member_array

`int member_array(mixed item, mixed *arr, int start = 0)`

`int member_array(int codepoint, string text, int start = 0)`

Return the index of the first element of `arr` equal to `item`, searching
from index `start`, or `-1` when there is none. Equality is that of `==`:
strings match by content, arrays, mappings and objects by identity, and a
destructed object compares as the configured [null value](../lpc/null.md).
Ints and floats match by [exact numeric value](../lpc/comparisons.md), so
`member_array(1, ({ 1.0 }))` returns `0`.
A `start` at or past the end finds nothing; a negative `start` is an error.

For strings, search for an integer Unicode scalar value. Both `start` and the
returned position count scalars, matching string [indexing](../lpc/indexing.md)
and `sizeof`. Combining marks count separately, and no normalization is performed.
This overload searches for a single character; use [strindex](strindex.md) to
search for a substring.

Integer zero searches for an actual NUL character. There is no implicit string
terminator to match, and embedded NULs do not stop the search. Non-integer needles
and invalid scalar integers (negative values, surrogates, or values above
`0x10ffff`) raise errors, even for empty text or a start past the end. The haystack
must be an array or string, and `start` must be an integer.

### Examples

```c
member_array("b", ({ "a", "b", "c" }));    // 1
member_array(9, ({ 1, 2, 3 }));            // -1
member_array('y', "xyzzy", 2);             // 4
member_array(0x732b, "é猫🐉猫", 2);          // 3
member_array(0, "abc");                   // -1
```

### See also

[sizeof](sizeof.md), [strindex](strindex.md), [map](map.md)
