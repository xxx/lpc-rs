# query_system_reload

`mapping query_system_reload(int id)`

Return a reload request's status, or 0 if the ID is unknown or expired. Only the
object that requested the reload can query it; other objects receive a permission
error. Requests become queryable after the requesting transaction commits.

| Key | Value |
| --- | --- |
| `target` | `"master"`, `"simul_efun"`, or `"both"` |
| `state` | `"queued"`, `"running"`, `"succeeded"`, or `"failed"` |
| `error` | Diagnostic text on failure; otherwise an empty string |

Status is an observation of the administrative job at query time. It may advance
while the querying task runs. The driver retains the latest 128 completed request
IDs plus queued and running requests. IDs are positive and may have gaps from
aborted attempts. They last for the current driver invocation.

### See also

`request_system_reload`, `valid_reload`
