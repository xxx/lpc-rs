# test_bit

`int test_bit(string s, int n)`

Return 1 when bit `n` of `s` is set, 0 when it is clear or past the end of
`s`.

Bit strings pack six bits per character, counted up from `" "` (value 0) to
`"_"` (63), the lowest bits in the first character; bit `n` is bit `n % 6` of
character `n / 6`. A character outside that range, a negative `n`, or an
`n` above 49152 is an error.

### Examples

```c
test_bit("_", 5)  /* 1 */
test_bit(" ", 3)  /* 0 */
```

### See also

`set_bit`, `clear_bit`
