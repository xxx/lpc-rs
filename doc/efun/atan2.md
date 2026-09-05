# atan2

`float atan2(int | float y, int | float x)`

Return the angle of the point `(x, y)` from the positive x axis, in radians
in `-pi..pi`. This is `atan(y / x)` with the quadrant kept, so `atan2(1, 0)`
is `pi / 2` and `atan2(-1, -1)` is `-3 * pi / 4`. Note the argument order,
`y` first, as in C. Int arguments are promoted; a non-number is an error.

### See also

`atan`, `tan`
