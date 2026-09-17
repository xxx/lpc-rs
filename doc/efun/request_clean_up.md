# request_clean_up

`int request_clean_up()`

Re-enable automatic `clean_up` queries in the calling object after it opted
out by returning zero. This change commits with the calling transaction;
aborted attempts do not re-enable queries.

Returns `1` when the request is accepted, including when queries are already
enabled. Returns `0` without changing cleanup state if the object declares
`#pragma resident`, lacks a `clean_up` hook, is the master or simul-efun
object, or automatic cleanup is disabled with `CLEAN_UP_INTERVAL=0`.

The driver still waits for the idle interval, and the hook's zero return wins
over a request accepted inside that same hook. A `1` return does not mean
the hook ran or the object was destroyed.

### See also

[`clean_up`](../apply/object/clean_up.md)
