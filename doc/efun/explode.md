# explode

`string *explode(string str, string delim = " ")`

Split `str` into substrings, using `delim` as the delimiter. If `delim` is not
given, it defaults to a single space. `delim` is not included in the result.
Matches are consumed from left to right without overlapping.

With a nonempty delimiter, empty fields are preserved, including those before a
leading delimiter, between consecutive delimiters, and after a trailing
delimiter. An empty input string returns `({ "" })`.

```lpc
explode("a||b||", "||")  // ({ "a", "b", "" })
explode("|a||b", "|")    // ({ "", "a", "", "b" })
explode("", "%%")        // ({ "" })
explode("ab---", "--")   // ({ "ab", "-" })
```

If `delim` is the empty string, the result contains one entry per Unicode scalar
value, without leading or trailing empty entries. A combining mark is a separate
entry; this does not split on grapheme clusters. Empty input returns an empty
array:

```lpc
explode("ab", "")        // ({ "a", "b" })
explode("", "")          // ({})
```

Other drivers differ on empty fields, empty input and character boundaries;
see the [porting comparison](../driver-comparison.md#explode) for examples and
configuration-dependent behaviour.
