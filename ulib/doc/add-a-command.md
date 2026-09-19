# Add a wave command

This tutorial adds a command to the room and uses two connections to check it.
Start with a working copy of ulib and complete the [first chat](../README.md#start-your-first-chat-world)
if you have not already done so.

## Register the verb

Open [std/room.c](../std/room.c). Inside `init()`, after the existing
`add_action()` calls, add:

```c
add_action("do_wave", "wave");
```

`init()` runs for a living entering the room. Registration associates that
player's `wave` verb with a function on this room.

## Supply the behaviour

Add this function outside `init()`, alongside `do_say()` and `do_emote()`:

```c
private int do_wave(string argument) {
    object player = this_player();
    write("You wave.\n");
    announce(capitalize(player->query_name()) + " waves.\n", player);
    return 1;
}
```

`write()` sends the speaker their own wording. `announce()` delivers the other
wording to everyone else in the room; its second argument excludes the speaker.
The `1` return value tells command dispatch that the verb was handled.

The function does not need the argument text, but accepts it because action
handlers receive the text after the verb. There is no object lookup, permission
check or user-supplied text in this action.

## Try it

Stop and restart the driver from your mudlib directory:

```sh
lpc-rs-driver --env driver.env
```

Connect as Alice and Bob again. Type `wave` as Alice. Alice should see
`You wave.` and Bob should see `Alice waves.`. Both should still be able to use
`say` and `quit`.

Add `wave` to the help text in [obj/player.c](../obj/player.c) so players can
discover it. Restart once more to load that edit.

You have added a command whose availability follows the player's location.
For a personal command available in every room, follow the existing `who`
implementation: register it on the player during name selection and put its
handler on that same object.
