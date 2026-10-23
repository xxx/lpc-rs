# Make objects players can use

In [your first program](first_program.md), a lantern spent fuel during one
run of the command-line runner. Now you will make two lantern objects in a
room and let players choose which one to burn. Their fuel will persist
between commands while the server runs.

## Start a small game

From the root of your lpc-rs checkout, install the server:

```sh
cargo install --locked --path lpc-rs-driver
```

Copy the bundled mudlib to a new working directory:

```sh
cp -R ulib ~/lantern-mud
cd ~/lantern-mud
lpc-rs-driver --env driver.env
```

Use a destination that does not already exist so the copy has the expected
layout. Keep this server terminal open. In a second terminal, connect with a
Telnet client or a MUD client pointed at `127.0.0.1`, port `24960`:

```sh
telnet 127.0.0.1 24960
```

Enter `alice` at `Name:`. You should arrive in **The Common Room** with an
`alice> ` prompt. Type `look`, then `say Hello!`. These are player commands
sent to the running game.

The [ulib guide](../../ulib/README.md) has more startup and troubleshooting
details. It supplies the login, player objects, and permissions used here.

Stop the server with Ctrl-C in its terminal before editing the files below.
All file paths in the rest of this lesson are relative to `~/lantern-mud`.

## Define a reusable lantern

Create `obj/lantern.c` with this complete program:

```lpc
#pragma strict_types

private int fuel = 3;

int burn() {
    if (fuel == 0) {
        return 0;
    }

    fuel = fuel - 1;
    return 1;
}

int query_fuel() {
    return fuel;
}
```

Each lantern will have its own `fuel` variable. A successful `burn()` spends
one unit and returns `1`; an empty lantern returns `0`. `query_fuel()` lets
other code read the amount without directly accessing the private variable.
The `query_` prefix is a naming convention for a function that reports state.

These functions have default public visibility, so another object can call
them. There is no `create()` here: initializing the global to `3` is all the
setup the lantern needs. This example uses a no-argument `burn()` in place of
the amount-taking function from the first lesson.

## Give the room two lanterns

Replace the entire contents of `room/lounge.c` in your copy with:

```lpc
#pragma strict_types, resident
inherit "/std/room";

private object left_lantern;
private object right_lantern;

void create() {
    ::create();
    set_short("The Lantern Room");
    set_long("Two lanterns sit on a table, one left and one right.\n"
        "Try 'fuel', 'burn left', or 'burn right'.\n");

    left_lantern = clone_object("/obj/lantern");
    right_lantern = clone_object("/obj/lantern");
}

void init() {
    ::init();
    add_action("do_fuel", "fuel");
    add_action("do_burn", "burn");
}

private int do_fuel(string argument) {
    write("Left: " + left_lantern->query_fuel()
        + "; right: " + right_lantern->query_fuel() + "\n");
    return 1;
}

private int do_burn(string argument) {
    if (argument != "left" && argument != "right") {
        write("Try 'burn left' or 'burn right'.\n");
        return 1;
    }

    object lantern = left_lantern;
    if (argument == "right") {
        lantern = right_lantern;
    }

    if (lantern->burn()) {
        write("The " + argument + " lantern glows.\n");
    } else {
        write("The " + argument + " lantern is empty.\n");
    }
    return 1;
}
```

Save both files. There are three new ideas here: reusing a room program,
creating objects, and registering commands. We will check the result first,
then follow each part.

The additional [`resident` pragma](pragmas.md#resident) keeps this tutorial
room out of ulib's automatic idle cleanup, so it keeps its lantern references
when everyone leaves. It does not save the room across server restarts.

## Use the lanterns

In the server terminal, still in `~/lantern-mud`, restart:

```sh
lpc-rs-driver --env driver.env
```

Reconnect as Alice. You should see **The Lantern Room** and the new
description. Type the following commands at the game prompt; the response
appears below each command:

```text
alice> fuel
Left: 3; right: 3
alice> burn left
The left lantern glows.
alice> fuel
Left: 2; right: 3
alice> burn left
The left lantern glows.
alice> burn left
The left lantern glows.
alice> burn left
The left lantern is empty.
alice> fuel
Left: 0; right: 3
```

Try `burn right`. It should still work. Try `burn` with no argument, then
`burn middle`; both should show the usage message. The existing `look`,
`say`, and `quit` commands should still work too.

If the room does not load, read the diagnostic in the server terminal and
check its file and line number. If the old room description appears, check
that you edited the copied `room/lounge.c` and restarted the server from
that copy's directory. Typing a command at the game prompt does not evaluate
LPC source; edit files in your text editor.

## Follow the objects

`object` is the type of a reference to a running LPC object.
`clone_object("/obj/lantern")` makes a fresh object from the program in
`obj/lantern.c`. The leading slash names the root of the **mudlib**, which
`driver.env` sets to this working directory. It does not name the host
computer's filesystem root. The `.c` extension can be omitted in this path.

The driver first loads the program's **prototype**, the object named
`/obj/lantern`. Each call to `clone_object` then creates a **clone** sharing
that compiled code and initializing its own variables. A clone starts with
the program's initial values, rather than copying changes to the prototype's
current variables.

| Expression | What it refers to |
|---|---|
| `"/obj/lantern"` | A string naming the lantern program's prototype |
| `left_lantern` | The first clone, with its own fuel |
| `right_lantern` | The second clone, with its own fuel |
| `lantern` inside `do_burn` | A local reference to the selected existing clone |

Assigning `left_lantern` to the local variable `lantern` copies the reference.
It does not make a third lantern. Calling `lantern->burn()` therefore changes
the selected lantern's fuel. The arrow means “call this function on that
object.” The function executes using that object's variables.

For this lesson the room holds references to the lanterns and offers commands
to use them. Its description supplies their presence in the scene. We have
not implemented picking up items, moving them, or a general inventory display;
those are additional game rules a mudlib supplies.

## Follow the inheritance

`inherit "/std/room";` reuses the variables and functions defined by
`std/room.c`. That file supplies the existing `look`, `say`, and `emote`
behaviour and the functions that set the room's descriptions. The lounge
adds its lantern references and commands to that program.

The lounge defines its own `create()` and `init()`. `::create()` explicitly
calls the inherited room's `create()`, and `::init()` calls the inherited
room's `init()`. We call the parent before adding our own setup so both sets
of behaviour run. Without `::init()`, our override would omit registration
of the base room's commands.

`set_short` and `set_long` are ordinary functions from the inherited room.
They are `protected`, which makes them available to the room and its
inheriting code. The code calls them directly because that inherited code is
part of this object.

The division is useful: put shared behaviour in a base program, then write
the smaller programs that configure or extend it. Creating a clone makes
another object; inheriting builds a program from existing code.

## Follow a command

When a player enters the room, the driver calls the room's `init()` to let it
offer commands to that player. `create()` was the one-time setup for the
room; `init()` can run again for other arriving players.

`add_action("do_burn", "burn")` connects a player command word, `burn`, to
the room's `do_burn` function. Typing `burn left` calls that handler with
`"left"` as its argument. Typing only `burn` supplies `0`: LPC uses zero
for an absent value, including an absent string or object reference.

The handler checks the argument before choosing an object. `!=` means “not
equal,” and `&&` means “and.” An argument that is neither `"left"` nor
`"right"` gets a usage message.

`write()` sends text to the player whose command is being handled. The room
runs the function, but the player receives the text. Both handlers are
`private` because they are implementation details of the room; the command
system can invoke a handler registered by its own object.

The final `return 1` means “this command was handled.” The empty-lantern and
usage cases also return `1`, because they already explained the outcome.
Returning `0` would let command dispatch keep looking for another handler.

Notice that `burn()` and `do_burn()` use their return values for different
purposes. `burn()` reports whether it spent fuel. `do_burn()` reports whether
it handled the player's command.

## Share the room with another player

Keep Alice connected. Open another client connection and enter `bob`. Type
`fuel` as Bob. He should see the same fuel amounts Alice left behind.

Alice and Bob have separate player objects, but they are using the same room
and the same two lanterns. Spending fuel as either player changes what the
other observes. The current handler sends its message only to the player
who typed the command; the next tutorial shows how to tell the other occupants.

Stop and restart the server, then reconnect. Both lanterns start at `3`
again. Restarting loads your latest source and creates new game objects;
this tutorial has no saved state.

## Try a change

Add a public `void refill()` function to `obj/lantern.c` that assigns `3` to
`fuel`. Then add a `refill` action to the room using `do_burn` as a guide:
validate `left` or `right`, select the corresponding object, call its
`refill()`, write a confirmation, and return `1`.

Restart and verify that `refill left` resets only the left lantern and that
an invalid argument still gets a useful message. Update the room description
to advertise your new command.

Continue with [Add a wave command](../../ulib/doc/add-a-command.md) to use
`this_player()` and notify other occupants. Return to the
[learning index](README.md) for reference material when you need it.
