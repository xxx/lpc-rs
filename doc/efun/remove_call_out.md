# remove_call_out

`int remove_call_out(int id)`

Remove a call out by its ID.

If the call out was successfully removed, milliseconds left before the call
would have been made are returned. If the call out was not found, or already removed,
-1 is returned.

Note that long-running instances will wrap the call out ID, so it is possible
to remove a call out that was not the one you intended, if you're
stashing the ID in a variable somewhere.

### Examples

```c
int call_out_id = call_out(&dump("repeats!"), 0, 0.1);

call_out(&remove_call_out(call_out_id), 0.5);
```

### See Also

`call_out`, `query_call_out`, `query_call_outs`