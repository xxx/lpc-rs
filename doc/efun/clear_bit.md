# clear_bit

`string clear_bit(string s, int n)`

Return `s` with bit `n` cleared; `s` itself is unchanged. A bit past the end
of `s` is already clear, so `s` comes back as it is.

Bit strings pack six bits per character, counted up from `" "` (value 0) to
`"_"` (63), the lowest bits in the first character; bit `n` is bit `n % 6` of
character `n / 6`. A character outside that range, a negative `n`, or an
`n` above 49152 is an error.

### Examples

```c
clear_bit("_", 5)   /* "?": 63 without bit 5 is 31 */
clear_bit("?<", 8)  /* "?8" */
```

### See also

`set_bit`, `test_bit`
