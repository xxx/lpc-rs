# json_decode

`mixed json_decode(string text)`

Parse JSON `text` into a value.

- An object becomes a mapping with string keys, in document order.
- An array becomes an array.
- A string becomes a string, escapes resolved.
- A number written without a fraction or exponent becomes an int; any other
  number, and an integer too large for an int, becomes a float.
- `true` becomes 1; `false` and `null` become 0.

Malformed text is an error naming the line and column, as is nesting deeper
than 128 levels.

### See also

`json_encode`, `gmcp`
