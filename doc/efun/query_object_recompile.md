# query_object_recompile

`mapping query_object_recompile(int id)`

Return the status of a committed `request_object_recompile` request, or 0 for an
unknown ID. Only the object that requested it may inspect it; another requester
raises a permission error.

The mapping contains:

| Key | Value |
| --- | --- |
| `target` | The original prototype, or 0 if it is no longer live. |
| `state` | `"queued"`, `"running"`, `"succeeded"`, or `"failed"`. |
| `updated` | Number of objects upgraded on success, including the prototype; otherwise 0. |
| `error` | Diagnostic text on failure; otherwise an empty string. |

Status is driver job state, observed when queried. The most recent 128 completed
requests are retained, along with pending requests. IDs from aborted transactions
may be skipped. Before the requesting transaction commits, its ID is unknown.

### See also

`request_object_recompile`, `valid_recompile`
