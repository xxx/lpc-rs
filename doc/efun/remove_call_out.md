# remove_call_out

`int remove_call_out(int id)`

Remove a call out by its ID.

Call outs scheduled by the same transaction can be removed; the removal
cancels their deferred scheduling, so they never materialize. Call outs
already committed are removed when this transaction commits.

If the call out was successfully removed, milliseconds left before the call
would have been made are returned. If the call out was not found, or already removed,
`-1` is returned, including when it was removed earlier in the same transaction.
If the transaction aborts, its cancellation is discarded and the committed call
out remains scheduled.

Note that long-running instances will wrap the call out ID, so it is possible
to remove a call out that was not the one you intended, if you're
stashing the ID in a variable somewhere.

### Examples

Run this from a player command, where `this_player()` identifies the recipient.
Capture the recipient explicitly so delivery works independently of
[`call_out`](call_out.md)'s player-preservation setting.

```c
object player = this_player();
int call_out_id = call_out(&tell_object(player, "repeats!\n"), 0, 0.1);

call_out(&remove_call_out(call_out_id), 0.5);
```

### See Also

`call_out`, `query_call_out`, `query_call_outs`
