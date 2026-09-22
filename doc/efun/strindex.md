# strindex

`int strindex(string text, string | int needle, int reverse = 0)`

Return the zero-based position of the first occurrence of `needle` in `text`,
or `-1` if it is absent. Any nonzero `reverse` value selects the last occurrence;
the result still counts from the beginning. The third argument is a direction
flag, not a starting position.

Matching is literal and case-sensitive. An integer needle is a Unicode scalar
value. Positions count Unicode scalar values, matching `sizeof` and string
[indexing](../lpc/indexing.md); combining marks count separately. No Unicode
normalization is performed.

An empty string needle matches position `0`, or `sizeof(text)` for a reverse
search. Both return `0` for empty text. Integer `0` searches for an actual NUL
character; there is no implicit terminator to match. Embedded NULs do not stop
the search.

Wrong argument types and invalid scalar values (negative integers, surrogates,
or values above `0x10ffff`) raise errors, including when `text` is empty.

```c
strindex("abcabc", "bc")       // 1
strindex("abcabc", "bc", -1)   // 4
strindex("abcabc", 'b', 1)     // 4
strindex("abc", "xyz")        // -1
strindex("é猫🐉", "🐉")       // 2
strindex("abc", "", -1)       // 3

string path = "/rooms/market";
string name = path[strindex(path, '/', -1) + 1..]; // "market"
```

See also: [replace_string](replace_string.md), [regmatch](regmatch.md),
[member_array](member_array.md).
