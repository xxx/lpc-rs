# get_mud_stats

`mapping get_mud_stats()`

The driver applies `get_mud_stats` in the master object the first time a
client asks for MSSP (the Mud Server Status Protocol — crawlers and mud lists
use it). The mapping's keys are MSSP variable names; a value is a string, an int,
or an array of strings for a multi-valued variable. Any other value is skipped
with a line in the debug log.

The driver always supplies `NAME` (`"lpc-rs"`), `PLAYERS` (logged-in
connections), `UPTIME` (boot time, unix seconds), `PORT` and `CODEBASE`; the
mapping overrides any of them and adds the rest. A master without the apply,
one that returns something other than a mapping, or one that errors, leaves
the defaults alone. Set `NAME` here.

The apply runs at most once per connection: its answer is reused for every
later request on that connection, so a client toggling MSSP off and on cannot
make the master work again. The driver's own values are rebuilt for each
reply, so `PLAYERS` and `UPTIME` are always current. While the driver is
shutting down the apply is skipped and the defaults are sent alone.
`this_player()` is 0 inside it.

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
