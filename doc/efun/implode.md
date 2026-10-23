# implode

`string implode(mixed *arr, string delim = " ")`

Join the elements of `arr` into a string, using `delim` as the delimiter. If
`delim` is not given, it defaults to a single space. If `delim` is the empty
string, the elements are joined without any delimiter.

Each element is converted to its string representation before joining. Integers
and nil values are included: `implode(({ "a", nil, 7, "b" }), "|")` returns
`"a|nil|7|b"` in distinct nil mode. An empty array returns `""`.

### See also

[explode](explode.md)
