# round

`float round(int | float x)`

Return the nearest whole number to `x`, as a float. Halfway values round away
from zero: `round(2.5)` is `3.0`, and `round(-2.5)` is `-3.0`. Values below the
halfway point round toward the nearer integer: `round(2.4)` is `2.0`.

An int argument is promoted to float before rounding; large integers can lose
precision during that conversion. The result is always a float, including for
integer inputs. Use `to_int(round(x))` when an int result is required, subject
to the integer range. Signed zero is preserved. A non-number is an error.

### See also

`floor`, `ceil`, `to_int`
