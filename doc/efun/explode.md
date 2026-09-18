# explode

`string *explode(string str, string delim = " ")`

Split `str` into substrings, using `delim` as the delimiter. If `delim` is not
given, it defaults to a single space. `delim` is not included in the result.

Empty fields are preserved, including those before a leading delimiter, between
consecutive delimiters, and after a trailing delimiter. With a nonempty delimiter,
an empty input string returns `({ "" })`.

```lpc
explode("a||b||", "||")  // ({ "a", "b", "" })
explode("|a||b", "|")    // ({ "", "a", "", "b" })
explode("", "%%")        // ({ "" })
```

If `delim` is the empty string, the result contains one entry per Unicode
character, with an empty string at each end:

```lpc
explode("ab", "")        // ({ "", "a", "b", "" })
```
