# query_object_recompile

`mapping query_object_recompile(int id)`

Return the status of a committed `request_object_recompile` request, or 0 for an
unknown ID. The original requesting object may inspect it directly. Other
objects may inspect it when the active master's
`valid_recompile(prototype, caller, program)` allows the querying object for each
target. A zero result or missing hook raises `object update status: permission
denied`; errors thrown by the hook propagate to the query.

The hook receives the querying object and its calling program, with that query's
`this_player()` and caller chain. It does not inherit the original requester's
authority. System selectors resolve to the current configured prototypes;
`"both"` checks the simul-efun prototype and then the master. A destroyed object
target or missing system prototype is passed as 0, so policy can still grant
access to failed jobs. An object target never resolves to a replacement at the
same path. Unknown IDs return 0 without calling the hook.

Authorization runs in the querying transaction. Reading status does not compile
or initialize anything and does not require the target to remain eligible for
recompilation. The original requester retains direct access even if permission
has since been revoked or its target has disappeared.

The mapping contains:

| Key | Value |
| --- | --- |
| `target` | The supplied system selector, or the original prototype (0 if it is no longer live). |
| `state` | `"queued"`, `"running"`, `"succeeded"`, or `"failed"`. |
| `updated` | Number of objects upgraded on success, including every selected prototype; otherwise 0. |
| `error` | Diagnostic text on failure; otherwise an empty string. |

Status is driver job state, observed when queried. The most recent 128 completed
jobs are retained, along with pending jobs. IDs from aborted transactions may be
skipped. Before the requesting transaction commits, its ID is unknown.

### See also

`request_object_recompile`, `valid_recompile`
