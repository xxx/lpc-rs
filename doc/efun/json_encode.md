# json_encode

`string json_encode(mixed value)`

Render `value` as JSON text, compact, with no whitespace between tokens.

- An int or float becomes a number. A float that is not finite is an error.
- A string becomes a string, escaped.
- An array becomes an array, element by element.
- A mapping becomes an object in insertion order. A string key is written as
  it is, an int or float key as its text; any other key is an error.
- A destructed object becomes `0`. A live object or a function pointer is an
  error.

Nesting deeper than 128 levels is an error, so a value that contains itself
does not loop. LPC has no `true`, `false` or `null`, and none is ever
written: `0` is a number.

### See also

`json_decode`, `send_gmcp`
