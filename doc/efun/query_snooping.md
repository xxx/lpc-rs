# query_snooping

`object query_snooping(object observer)`

Return the target watched by `observer`, or the configured native null when
there is none. Null or destructed observers return null without authorization.
Every live observer, including an unattached one, requires the master's
[`valid_snoop`](../apply/master/valid_snoop.md) permission with operation
`"query_snooping"`. Refusal raises an error.

Reads use the calling task's transactional view. A failed delivery or queue
overflow can remain visible until its automatic cleanup commits.

### See also

[`snoop`](snoop.md), [`query_snoop`](query_snoop.md)
