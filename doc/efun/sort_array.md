# sort_array

`mixed *sort_array(mixed *arr, function compare)`
`mixed *sort_array(mixed *arr, int direction)`

Return a new array holding `arr`'s items in sorted order; `arr` itself is
not changed. The sort is stable: items that compare equal keep their
order.

With a function, `compare(a, b)` is called for pairs of items and must
return an int: negative to put `a` before `b`, positive to put `a` after
`b`, zero for either order. A non-int result is an error, and an error the
function throws is `sort_array`'s caller's. The function runs as a call
from the object calling `sort_array`, on that task's own call stack, so
nesting is bounded by the call stack rather than the task chain.

With an int, the items are sorted in their natural order: ints and floats
by value, together, and strings by content. A `direction` of `-1` sorts
descending; any other value ascending. Two items natural order cannot
rank against each other, such as an int and a string, are an error.

### Examples

```c
sort_array(({ 3, 1, 2 }), (: $1 - $2 :));        // ({ 1, 2, 3 })
sort_array(({ "b", "a" }), 1);                    // ({ "a", "b" })
sort_array(objects, (: $1->query_level() - $2->query_level() :));
```

### See also

`filter`, `map`, `min`, `max`
