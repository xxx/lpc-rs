# logon

`int logon(string remote_ip_addr, int remote_port)`

The driver applies `logon` to the object that is returned by the call to `connect`
that was made for this user. The return value determines what happens next.

If `0` is returned, the user is disconnected. If `1` is returned, the user is
not disconnected. At its core, that's all this function does.

A client that goes away after `logon` — or during it, once `connect` has
returned an object — is unbound from that object, which then receives
`net_dead`.

During `logon`, `this_player` and `this_interactive` are now available for use,
though commands have not been enabled yet (this is the place to do it, though).

Implementing `logon` is _not_ optional, and the user will be disconnected if the
object does not implement it.

Output and `input_to` registrations are delivered in call order after the
`logon` transaction commits. An uncaught error discards both; conflict retries
deliver only the successful attempt's operations.

Typed input waits until `logon` succeeds and its output and callback
registrations have been delivered. A client can reply as soon as it sees a
prompt without racing a later `input_to` or `get_char` registration. Telnet
protocol events, including GMCP and window-size updates, can still arrive
during login.

### Examples

```c
string name;

int logon(string remote_ip_addr, int remote_port) {
    write("Welcome to the game!\n");
    write("What's your name?");
    input_to(get_name);
    return 1;
}

private int get_name(string new_name) {
    name = new_name;
    enter_game();
    return 1;
}

private void enter_game() {
    write("Hello, " + name + "!");
    enable_commands(); // or exec() them into a new body with their real commands.
    move_object("/my_start_room");
}
```

### See also

`connect`, `exec`, `enable_commands`, `net_dead`
