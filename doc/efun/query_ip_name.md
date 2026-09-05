# query_ip_name

`string query_ip_name(object ob = this_player())`

Return the host name of `ob`'s connection, or `0` when it has none. The
driver starts a reverse DNS lookup when a client connects; until it
answers, and forever when the address has no name, the result is the
address, the same string `query_ip_number` returns. The answer is cached
on the connection, so the call itself never waits.

### Examples

```c
write("You are connecting from " + query_ip_name() + ".\n");
```

### See also

`query_ip_number`, `query_connection`
