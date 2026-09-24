# snoop

`object snoop(object observer, object target = nil)`

Attach `observer` to the decoded input and accepted text output of `target`'s
connection. Return `target`. The target must be a live interactive object;
the observer must be live and implement
[`receive_snoop`](../apply/object/receive_snoop.md), but need not be interactive
or living. Observations are delivered asynchronously through that apply.

Omitting `target`, or passing the configured native null, stops observation and
returns `observer`, including when it was already detached. Each observer can
watch one target, and each target can have one observer. Attaching the same pair
is idempotent. An occupied endpoint, self-observation, a cycle, an invalid
endpoint or an incompatible callback raises an error. Stop explicitly before
changing partners; stop and start may share one transaction.

Every call requires the master's
[`valid_snoop`](../apply/master/valid_snoop.md) authorization, including an
idempotent start or stop. Refusal raises `snoop: permission denied`; policy
errors propagate. Relationships and controls commit with the calling task.

Capture begins when the connection processes the committed start. Earlier text
is not replayed. Stopping invalidates queued and uncommitted callbacks; effects
from an already committed callback may still arrive. Stop and reattach creates
a new registration, even for the same pair. Starting, writing and stopping in
one transaction creates no capture window.

Destruction of either object, target disconnection, or transfer or replacement
of the target's connection ends the relationship. An observer's disconnection
alone does not end it. Recompiling the same object can preserve it when the
callback remains compatible; a replacement object does not inherit it.

Callback failure or queue overflow also ends observation without disconnecting
the target. See the delivery apply for text, context and queue limits.

### See also

[`query_snoop`](query_snoop.md), [`query_snooping`](query_snooping.md)
