# notify_fail

`int notify_fail(string | function message)`

Sets the message the player sees if nothing handles the command in progress.
A function is called with no arguments when the message is needed; a string it
returns is the message, anything else means it reported the failure itself.
The last call wins. Returns 0, so `return notify_fail("...");` reads as "not
handled".

### Examples

```c
int do_open(string what) {
    if (what != "door") {
        return notify_fail("Open what?\n");
    }

    write("You open the door.\n");
    return 1;
}
```

### See also

`query_notify_fail`, `command_not_found`, `add_action`
