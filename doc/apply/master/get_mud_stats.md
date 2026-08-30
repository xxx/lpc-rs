# get_mud_stats

`mapping get_mud_stats()`

The driver applies `get_mud_stats` in the master object each time a client
asks for MSSP (the Mud Server Status Protocol — crawlers and mud lists use
it). The mapping's keys are MSSP variable names; a value is a string, an int,
or an array of strings for a multi-valued variable. Any other value is skipped
with a line in the debug log.

The driver always supplies `NAME` (`"lpc-rs"`), `PLAYERS` (logged-in
connections), `UPTIME` (boot time, unix seconds), `PORT` and `CODEBASE`; the
mapping overrides any of them and adds the rest. A master without the apply,
one that returns something other than a mapping, or one that errors, leaves
the defaults alone. Set `NAME` here.

### Examples

```c
mapping get_mud_stats() {
    return ([
        "NAME": "Jesters Court",
        "CONTACT": "admin@example.org",
        "FAMILY": "LPMud",
        "PORTS": ({ "4000", "4001" }),
    ]);
}
```
