# valid_snoop

`int valid_snoop(object caller, object observer, object target, string operation, string program)`

Authorize observation or inspection of a connection's snoop relationship.
Return nonzero to permit; zero, nil or omission denies with an error. Apply
errors propagate. There is no implicit permission for inspecting oneself.

| Operation | Observer | Target |
|---|---|---|
| `"start"` | Supplied observer | Supplied target |
| `"stop"` | Supplied observer | Current target, or nil |
| `"query_snoop"` | nil | Supplied query endpoint |
| `"query_snooping"` | Supplied query endpoint | nil |

`caller` is the executing object. `program` names the source defining the
request, as an absolute in-game path with `.c`, or nil when unavailable.
An inherited method or simul-efun names its defining program. `nil` means the
configured native null throughout this contract.

The apply runs in the requesting transaction, with the master as
`this_object()` and the caller's player context preserved. It runs even for
idempotent starts/stops and live but unattached query endpoints. Queries of
null or destructed endpoints return nil without entering the apply.
Automatic cleanup requires no grant. The driver rechecks mutable relationship
state after authorization. Calling these inspectors from this policy invokes
the policy again; use the supplied arguments or your own policy records.

### Example

```c
int valid_snoop(object caller, object observer, object target,
                string operation, string program) {
    return program == "/secure/snoop_service.c"
        && caller == find_object("/secure/snoop_service");
}
```

The trusted service must enforce its own user/consent policy before making the
request. Permission to compile `efun::snoop` is separate from this runtime grant.

### See also

[`snoop`](../../efun/snoop.md), [`receive_snoop`](../object/receive_snoop.md)
