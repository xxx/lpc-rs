# to_int

`int to_int(mixed value)`

Convert `value` to an int:

- An int is returned as it is.
- A float is truncated toward zero: `to_int(-2.7)` is `-2`.
- A string yields its leading integer: leading whitespace is skipped, then
  an optional sign and decimal digits are read, and the rest is ignored.
  `to_int("12abc")` is `12`; a string with no leading digits is `0`. Digits
  past the int range are an error.
- A destructed object is `0`.

Anything else (a live object, an array, a mapping, a function) is an error.
There are no casts in lpc-rs; this is the conversion.

### See also

`to_float`, `to_string`, `sscanf`
