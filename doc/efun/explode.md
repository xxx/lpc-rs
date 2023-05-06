# explode

`string *explode(string str, string delim = " ")`

Split `str` into substrings, using `delim` as the delimiter. If `delim` is not
given, it defaults to a single space. If `delim` is the empty string, the string
is split into an array of characters. `delim` is not included in the result.
