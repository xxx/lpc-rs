# connect

`mixed connect(string remote_ip_addr, int remote_port)`

The driver applies `connect` upon first contact with a user. The return value
of `connect` determines what happens next to the user. 

If an object is returned, the user is `exec`ed into that object, which 
becomes their body, and `logon` is then applied in that object.

If a string is returned, it is printed to the user, and they are disconnected.

If anything else is returned, a generic error is printed to the user, and they are disconnected.

While `connect` is executing, `this_player` and `this_interactive` are not
available, and `this_object` is the master object.

Implementing `connect` is _not_ optional - it must be callable to be able to connect to the game.

### Examples

```c
mixed connect(string remote_ip_addr, int remote_port) {
    if (ip_blocks[remove_ip_addr]) {
        return "You are not allowed to connect to this game.";
    }
 
    // It's important to return a clone here, as objects can have only one user
    // attached to them at a time.
    return clone_object("/lib/login_handler");
}
```

### See also

`logon`, `exec`