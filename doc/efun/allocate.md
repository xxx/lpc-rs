# allocate

`mixed *allocate(int n, mixed init = nil)`

Return an array of `n` elements, each set to `init`, or to the selected null
value when `init` is omitted. With the default `NULL_MODE=zero`, that value is
integer `0`; [distinct mode](../lpc/null.md) uses `nil`.

Negative counts and sizes whose allocation layout cannot fit on the current
platform are runtime errors.

`init` is one value placed in every slot: when it is an array or a mapping,
every slot refers to that same array or mapping, so a change through one
slot shows through all of them. Build the rows separately when each needs
its own.

### Examples

```c
int *counts = allocate(10, 0);         // ten zeros in either null mode
string *names = allocate(3, "nobody"); // three copies of the string
```

### See also

`sizeof`
