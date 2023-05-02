# logon

`int logon(string remote_ip_addr, int remote_port)`

The driver applies `logon` to the object that is returned by the call to `connect`
that was made for this user. The return value determines what happens next.

If `0` is returned, the user is disconnected. If `1` is returned, the user is
not disconnected. At its core, that's all this function does.

During `logon`, `this_player` and `this_interactive` are now available for use,
though commands have not been enabled yet (this is the place to do it, though).

Implementing `logon` is _not_ optional, and the user will be disconnected if the
object does not implement it.

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

`connect`, `exec`, `enable_commands`