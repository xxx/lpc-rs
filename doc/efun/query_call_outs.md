# query_call_outs

`mixed *query_call_outs(object|int ob = this_object())`

Get information about scheduled call outs:

- Omit `ob`, or pass `nil`, to query the current object.
- Pass an object to query that object's call outs.
- Pass `-1` to query call outs across all objects.

Other integers are errors, except `0` in zero-null mode, where it has the same
meaning as `nil`. Other argument types are errors.

The query includes call outs scheduled in the current transaction, including
those scheduled by other objects called within it. Call outs cancelled in the
current transaction and those owned by destructed objects are excluded.
Uncommitted schedules from other transactions are not visible.

The result is an array of arrays. Each row has the same format as `query_call_out`:

```c
({ object, function, remaining_ms, repeat_ms, id })
```

The object at index `0` is the object that invoked `call_out`, which can differ
from the function pointer's owner. The ID at index `4` is the value returned by
`call_out`, so filtered rows can be passed to `remove_call_out`.

An empty result is `({})`. Row order is unspecified; sort the result when display
order matters. Remaining time describes the timer when queried and does not
reserve its execution.

### Examples

```c
dump(query_call_outs());
dump(query_call_outs(-1));

// Remove every repeating call out owned by this object.
mixed *repeating = filter(query_call_outs(), (: $1[3] > 0 :));
foreach (mixed *row : repeating) {
    remove_call_out(row[4]);
}
```

### See Also

`call_out`, `remove_call_out`, `query_call_out`
