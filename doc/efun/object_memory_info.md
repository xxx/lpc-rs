# object_memory_info

`mapping object_memory_info(object ob)`

Estimate the storage of a loaded object using the caller's transaction. Pending
global assignments, container changes and program upgrades are visible. The
result is a fresh mapping containing four integers, in bytes:

| Key | Estimate |
| --- | --- |
| `object_bytes` | Object and current program-image headers, global handles and value slots, and any virtual object name. |
| `program_bytes` | The current compiled program's tables, executable bodies, buffers and declaration metadata. |
| `data_bytes` | LPC storage reachable through globals, bound function arguments and captured cells. |
| `total_bytes` | The sum of the preceding three fields. |

Program estimates are cached on the immutable compiled program and reused by its
clones. All bindings of one executable body count it once within the program
estimate.

The data walk includes array elements, mapping keys and values, class fields,
strings, byte sequences, functions and their captures. Shared containers and
function/string identities are visited once per call, and cycles terminate.
Ordinary object references contribute their existing value slots; the target's
state is not traversed. Numbers and null values have no additional payload cost.
Function executables and retained images outside the current program are excluded.

Array, class and function buffers use their retained capacities. Hash-table storage
is estimated from capacity and entry sizes. Dynamic strings use capacity; interned
string text is counted once by its interned identity within the data walk. Byte
sequences use visible slice lengths, with identical slices counted once;
overlapping slices can count overlapping bytes. These are build-dependent
estimates, not allocator measurements or strict upper bounds.

The estimates exclude allocator bookkeeping, global intern-pool storage for program
symbols/literals, source/compiler caches, historical program images retained by
functions, MVCC history, active task stacks and separately allocated native resources
such as connections, parser rules and timers. Class slot/view buffers count as data;
compiled class definitions count with the current program. Retained definitions
outside that program are not separately attributed.

Each object's row includes its full program estimate and reachable data even when
another object shares them. Rows cannot be added into a driver total, and the
result does not predict the memory freed by destructing the object.

The configured null or a destructed target returns an empty mapping. Other argument
types are errors. The efun does not load objects, call LPC authorization hooks,
reveal values or modify the measured object's state.

The walk yields periodically so the task's execution timeout can cancel it. More
than 1,000,000 traversal steps raises `object_memory_info: traversal limit exceeded`;
the count includes slots, values and container entries. A size that cannot fit an
LPC integer raises `object_memory_info: size overflow`. Failed calls return no
partial estimate.

```c
mapping memory = object_memory_info(this_object());
printf("Program: %d bytes; data: %d bytes\n",
    memory["program_bytes"], memory["data_bytes"]);
```

See also: [variable_info](variable_info.md),
[query_resident_memory](query_resident_memory.md),
[source_cache_stats](source_cache_stats.md), [rusage](rusage.md).
