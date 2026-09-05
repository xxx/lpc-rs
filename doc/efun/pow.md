# pow

`float pow(int | float x, int | float y)`

Return `x` raised to the power `y`, always as a float: `pow(2, 3)` is `8.0`
and `pow(8, 1.0 / 3)` is `2.0`. Zero to a negative power, a negative base to
a fractional power, and a result too large for a float are errors. Int
arguments are promoted; a non-number is an error.

### See also

`exp`, `log`, `sqrt`
