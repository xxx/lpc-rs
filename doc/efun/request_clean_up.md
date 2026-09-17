# request_clean_up

`void request_clean_up()`

Re-enable automatic `clean_up` queries in the calling object after it opted
out by returning zero. This change commits with the calling transaction;
aborted attempts do not re-enable queries.

The driver still waits for the idle interval, and the hook's zero return wins
over a request made inside that same hook. This efun does not call the hook
immediately, add a missing hook, or override `CLEAN_UP_INTERVAL=0`.

### See also

[`clean_up`](../apply/object/clean_up.md)
