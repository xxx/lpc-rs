# implode

`string implode(mixed *arr, string delim = " ")`

Join the elements of `arr` into a string, using `delim` as the delimiter. If
`delim` is not given, it defaults to a single space. If `delim` is the empty
string, the elements are joined without any delimiter.

Any non-strings in `arr` are ignored.

### See also

`explode`