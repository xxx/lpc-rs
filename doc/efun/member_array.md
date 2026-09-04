# member_array

`int member_array(mixed item, mixed *arr, int start = 0)`

Return the index of the first element of `arr` equal to `item`, searching
from index `start`, or `-1` when there is none. Equality is that of `==`:
strings match by content, arrays, mappings and objects by identity, and a
destructed object is `0`. A `start` past the end finds nothing; a negative
`start` is an error, as is a non-array `arr`.

### Examples

```c
member_array("b", ({ "a", "b", "c" }));    // 1
member_array(9, ({ 1, 2, 3 }));            // -1
```

### See also

`sizeof`
