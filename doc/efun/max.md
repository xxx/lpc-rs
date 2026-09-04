# max

`int | float max(int | float x, ...)`
`int | float max(mixed *values)`

Return the largest of the numbers passed, or of the numbers in the one
array passed. Ints and floats may be mixed; the winner is returned as it
was, so `max(1, 2.5)` is the float `2.5`. An empty array, or a value that
is not a number, is an error.

### See also

`min`, `abs`
