# query_sockets

`mapping *query_sockets(object|int owner = this_object())`

List authorized socket snapshots for the current object (argument omitted or nil),
a selected live object, or all owners with `-1`. Other integers are errors, except
zero in zero-null mode where it is nil. A destructed object is an error and cannot
become an all-owner query. The master must authorize `query`, including self queries.

Each row has the `socket_info` mapping shape. Rows include this transaction's changes
and exclude retired records and hidden disposal charges. Empty results are `({})`;
row order is unspecified. Inspection does not grant write, close or transfer rights.

### See also

[TCP sockets and events](../tcp-sockets.md), [`valid_socket`](../apply/master/valid_socket.md)
