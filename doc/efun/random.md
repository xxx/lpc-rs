# random

`int random(int n)`

Return a uniformly distributed int in the range `0` to `n - 1`. `random(0)`
is `0`; a negative `n` is an error.

The generator is not part of the transaction: a task that retries draws a
fresh number on each attempt.

### Examples

```c
int roll = random(6) + 1;   // 1 to 6
```

### See also

`time`
