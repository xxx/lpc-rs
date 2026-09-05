# set_bit

`string set_bit(string s, int n)`

Return `s` with bit `n` set; `s` itself is unchanged. A bit past the end of
`s` extends it with spaces first: `set_bit("", 6)` is `" !"`.

Bit strings pack six bits per character, counted up from `" "` (value 0) to
`"_"` (63), the lowest bits in the first character; bit `n` is bit `n % 6` of
character `n / 6`. A character outside that range, a negative `n`, or an
`n` above 49152 is an error.

### Examples

```c
set_bit("?", 5)   /* "_": 31 with bit 5 is 63 */
set_bit("78", 8)  /* "7<" */
```

### See also

`clear_bit`, `test_bit`
