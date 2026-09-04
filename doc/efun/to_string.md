# to_string

`string to_string(mixed value)`

Convert `value` to a string:

- A string is returned as it is.
- An int or float becomes its text, as `"" + value` would render it.
- A live object becomes its file name, as `file_name` returns it; a
  destructed object becomes `"0"`.

An array, a mapping or a function is an error: use `dump` or `json_encode`
to render those.

### See also

`to_int`, `to_float`, `file_name`, `json_encode`
