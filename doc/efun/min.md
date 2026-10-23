# min

`int | float min(int | float x, ...)`
`int | float min(mixed *values)`

Return the smallest of the numbers passed, or of the numbers in the one
array passed. Ints and floats may be mixed; the winner is returned as it
was, so `min(1, 0.5)` is the float `0.5`. An empty array, or a value that
is not a number, is an error.
Numeric comparison is [exact across ints and floats](../lpc/comparisons.md);
the first value wins a tie.

### See also

`max`, `abs`
