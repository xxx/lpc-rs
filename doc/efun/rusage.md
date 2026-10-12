# rusage

`mapping rusage()`

Return a fresh mapping of operating-system resource statistics for the driver
process, including all its threads. The statistics include compilation, runtime
and other work in that process; they do not measure an individual LPC object or
VM. Child processes are excluded. CPU accounting starts with the process rather
than with the VM's [`uptime`](uptime.md).

Successful samples contain these 16 string keys with integer values:

| Key | Meaning and unit |
|---|---|
| `utime` | Accumulated user CPU time, in milliseconds. |
| `stime` | Accumulated system CPU time, in milliseconds. |
| `maxrss` | Peak process resident set size, in KiB (1,024 bytes). |
| `ixrss` | Integral shared-memory usage, in the OS's accounting units. |
| `idrss` | Integral unshared data-memory usage, in the OS's accounting units. |
| `isrss` | Integral unshared stack-memory usage, in the OS's accounting units. |
| `minflt` | Page faults serviced without I/O. |
| `majflt` | Page faults requiring I/O. |
| `nswap` | Swaps. |
| `inblock` | Block input operations. |
| `oublock` | Block output operations. |
| `msgsnd` | IPC messages sent. |
| `msgrcv` | IPC messages received. |
| `nsignals` | Signals received. |
| `nvcsw` | Voluntary context switches. |
| `nivcsw` | Involuntary context switches. |

Fractional milliseconds and KiB are discarded. Values outside the LPC integer
range saturate at its bounds. Fields the OS does not maintain stay at its reported
value, typically zero; all keys are still present. Integral-memory accounting
units and the availability of individual counters depend on the OS.

Sampling uses `getrusage(RUSAGE_SELF)` through a safe syscall wrapper on supported
Unix targets. A failed sample returns an empty mapping. Targets without the
backend also return an empty mapping; this currently includes non-Unix targets,
Redox, Fuchsia, Solaris and Haiku. An empty result means resource data is
unavailable, while a complete zero-valued sample is still a successful reading.

Each call samples again, including calls in a retried task. The OS reading is
outside the transaction snapshot, while the returned mapping is allocated in
the calling transaction. Changing it does not affect resource accounting or
later results.

Direct calls take no arguments. Calls through a function pointer evaluate and
drop unused arguments under the normal [function rules](../lpc/function_type.md).

```lpc
mapping usage = rusage();
if (sizeof(usage))
    write(sprintf("CPU time: %.3f seconds\n",
                  (usage["utime"] + usage["stime"]) / 1000.0));
```

### See also

[uptime](uptime.md), [query_resident_memory](query_resident_memory.md),
[source_cache_stats](source_cache_stats.md)
