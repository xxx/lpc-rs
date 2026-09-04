# to_float

`float to_float(mixed value)`

Convert `value` to a float:

- A float is returned as it is.
- An int is widened: `to_float(3)` is `3.0`.
- A string yields its leading number: leading whitespace is skipped, then an
  optional sign, digits, an optional fraction and an optional exponent are
  read, and the rest is ignored. `to_float("1.5abc")` is `1.5`; a string
  with no leading digits is `0.0`.
- A destructed object is `0.0`.

Anything else (a live object, an array, a mapping, a function) is an error.

### See also

`to_int`, `to_string`
