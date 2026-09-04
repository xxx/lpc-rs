# allocate

`mixed *allocate(int n, mixed init = 0)`

Return an array of `n` elements, each set to `init`, or to `0` when `init`
is not given. A negative `n` is an error.

`init` is one value placed in every slot: when it is an array or a mapping,
every slot refers to that same array or mapping, so a change through one
slot shows through all of them. Build the rows separately when each needs
its own.

### Examples

```c
int *counts = allocate(10);            // ten zeros
string *names = allocate(3, "nobody"); // three copies of the string
```

### See also

`sizeof`
