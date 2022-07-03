# debug

`mixed debug(string operation, ...)`

This is the kitchen-sink function used for interacting with lpc-rs from
within a game. Arguments differ based on `operation`.

### Operations

* `snapshot_stack` - Store a snapshot of the current stack. This operation is
  only available in test builds, and will return a runtime error if used
  in a development or release build. It's only used for testing lpc-rs itself.
  This operation has no arguments.