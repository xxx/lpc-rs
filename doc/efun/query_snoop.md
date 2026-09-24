# query_snoop

`object query_snoop(object target)`

Return the observer attached to `target`, or the configured native null when
there is none. Null or destructed targets return null without authorization.
Every live target, including an unattached one, requires the master's
[`valid_snoop`](../apply/master/valid_snoop.md) permission with operation
`"query_snoop"`. Refusal raises an error.

Reads use the calling task's transactional view. A failed delivery or queue
overflow can remain visible until its automatic cleanup commits.

### See also

[`snoop`](snoop.md), [`query_snooping`](query_snooping.md)
