# to_string

`string to_string(mixed value)`

Convert `value` to a string:

- A string is returned as it is.
- An int or float becomes its text, as `"" + value` would render it.
- A live object becomes its file name, as `file_name` returns it; a
  destructed object becomes `"0"`.

An array, a mapping or a function is an error: use `dump` or `json_encode`
to render those. A `bytes` is an error too: it is not text until an encoding
says so, which is what `to_text` takes.

### See also

`to_int`, `to_float`, `to_text`, `to_bytes`, `file_name`, `json_encode`
