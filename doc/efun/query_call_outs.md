# query_call_outs

`mixed *query_call_outs(object ob = this_object())`

Get information about all call outs of an object, defaulting to the object it's called within.
Call outs scheduled by the same transaction are included.

The result is an array of arrays. Each row has the same format as `query_call_out`:

```c
({ object, function, remaining_ms, repeat_ms, id })
```

The ID at index `4` is the value returned by `call_out`, so filtered rows can
be passed to `remove_call_out`. Call outs removed in the current transaction
are excluded.

### Examples

```c
dump(query_call_outs());

// Remove every repeating call out owned by this object.
mixed *repeating = filter(query_call_outs(), (: $1[3] > 0 :));
foreach (mixed *row : repeating) {
    remove_call_out(row[4]);
}
```

### See Also

`call_out`, `remove_call_out`, `query_call_out`
