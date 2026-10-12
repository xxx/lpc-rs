# uptime

`int uptime()`

Return whole seconds elapsed since this VM's state was created. Fractional
seconds are discarded. The clock is monotonic, so adjustments to the system
wall clock do not change the result. Each VM has its own startup time; loading,
cloning or recompiling LPC objects does not reset it.

Each call reads the clock again, including calls in a retried task. The reading
is not part of the transaction snapshot. Values saturate at the maximum LPC
integer rather than wrapping.

```lpc
int hours = uptime() / 3600;
```

### See also

[time](time.md), [object_time](object_time.md), [rusage](rusage.md)
