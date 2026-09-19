/**
 * One player clone owns a connection from name selection until departure.
 * The master returns this body from connect(); no separate login object or
 * exec() transfer is needed, and the name is never saved to disk.
 * no_inherit and no_shadow keep other objects from reusing or intercepting
 * this connection-handling code; ordinary game messages use query_name().
 * See ../doc/how-it-works.md for the login sequence and ../doc/applies.md
 * for the driver's living and special apply contracts.
 */
#pragma strict_types, no_inherit, no_shadow
#include <ulib.h>

// An uninitialised LPC string is 0; after login this holds a lowercase ASCII name.
private string name;

/**
 * Mudlib helper, not a driver apply: return this guest's lowercase name or 0
 * while name selection is unfinished; no arguments and no formatting.
 * users() includes unnamed bodies, so who, chat and name-uniqueness checks
 * use this accessor rather than treating every connected body as logged in.
 */
string query_name() {
    return name;
}

/**
 * Begin login after the driver attaches a connection to this body.
 * remote_ip_addr and remote_port are the client's address and source port;
 * this_player() is this body, and neither argument is used as an identity.
 * Return 1 to keep the connection while input_to() waits for a guest name;
 * a 0 result or a missing logon() causes the connection to be rejected.
 * Commands stay disabled until receive_name() accepts a unique name.
 */
int logon(string remote_ip_addr, int remote_port) {
    write("Welcome to " + MUD_NAME + "! Choose a temporary guest name.\n");
    write("Use " + MIN_NAME_LENGTH + "-" + MAX_NAME_LENGTH
        + " letters, or quit to disconnect.\nName: ");
    input_to(receive_name);
    return 1;
}

/**
 * input_to() callback, not an apply: consume one input line as a guest name.
 * input is the entered text; the callback's return value is ignored.
 * Each input_to() registration consumes only one line, so an invalid or
 * occupied name must register this callback again before returning.
 * A valid name enables personal actions and moves into the chat room;
 * "quit" instead removes this still-unnamed body and closes its connection.
 */
private void receive_name(string input) {
    string chosen = lower_case(input);
    if (chosen == "quit") {
        write("Goodbye!\n");
        destruct(this_object());
        return;
    }
    if (sizeof(chosen) < MIN_NAME_LENGTH || sizeof(chosen) > MAX_NAME_LENGTH
        || !regmatch(input, "^[A-Za-z]+$")) {
        write("Names must contain " + MIN_NAME_LENGTH + "-" + MAX_NAME_LENGTH
            + " letters, with no spaces.\nName: ");
        input_to(receive_name);
        return;
    }

    // These reads and our name assignment share a transaction, including conflict retries.
    foreach (object guest : users()) {
        if (guest != this_object() && guest->query_name() == chosen) {
            write("That name is already in use. Try another.\nName: ");
            input_to(receive_name);
            return;
        }
    }
    name = chosen;
    enable_commands();
    add_action(do_help, "help");
    add_action(do_who, "who");
    add_action(do_quit, "quit");

    // Movement calls the room's init(), which supplies look, say and emote.
    move_object(START_ROOM);
    write("Welcome, " + capitalize(name) + "! Type help for commands.\n");
    command("look");
    environment()->announce(capitalize(name) + " has joined.\n", this_object());
}

/**
 * Return the next prompt after logon(), a command or an input callback.
 * The driver delivers the string and a Telnet prompt marker; returning 0
 * supplies no text, and omitting the hook supplies neither text nor marker.
 * While input_to() is pending this hook is skipped and only the marker is
 * sent, so the name-selection code must write its own "Name: " prompt.
 */
string write_prompt() {
    return name ? name + "> " : 0;
}

/**
 * Deliver message text addressed to this object, for example by tell_object().
 * The message is already formatted by its sender; the return is ignored.
 * write_socket() sends it to our connection without invoking catch_tell()
 * again; using write() here would recurse through this same hook.
 * Omission uses the driver's connection/debug-log delivery fallback.
 */
void catch_tell(string message) {
    write_socket(message);
}

/**
 * Expand a personal alias before dispatching the incoming command line.
 * line is the command after any master modify_command() rewrite.
 * Return a replacement string for apostrophe-prefixed speech, 0 to dispatch
 * the original, or another non-string value to consume a line entirely.
 * Omission passes input through; input_to() name callbacks bypass this path.
 * Unlike this hook, the master's modify_command() consumes input on 0.
 */
mixed process_input(string line) {
    if (sizeof(line) && line[0..0] == "'") {
        return "say " + line[1..];
    }
    return 0;
}

/**
 * Answer whether a grammar noun phrase identifies this player.
 * phrase is the name being tested; return 1 for our name ignoring case,
 * or 0 for another name or an unfinished login.
 * The parser uses id() only when parse_command_id_list() is absent; without
 * either hook this body contributes no object-specific noun matches.
 * The master's shared ids do not apply to this fallback.
 */
int id(string phrase) {
    if (!name) {
        return 0;
    }
    return lower_case(phrase) == name;
}

/**
 * Personal add_action() handler for help, available wherever this player goes.
 * argument is the text after the verb or 0; this command ignores it.
 * Return 1 after printing so dispatch knows the command was handled.
 * Keep this list in sync when adding a room or player command.
 */
private int do_help(string argument) {
    write("Commands:\n"
        "  look          Describe the room.\n"
        "  who           List connected guests.\n"
        "  say <text>    Speak to the room (or use 'text).\n"
        "  emote <text>  Describe an action.\n"
        "  help          Show this list.\n"
        "  quit          Leave and release your name.\n");
    return 1;
}

/**
 * Personal action listing named, connected guests across the whole game.
 * argument is the unused command remainder or 0; return 1 to consume who.
 * Filter users() through query_name() because connections choosing a name
 * are not yet participants, then sort display names for stable output.
 */
private int do_who(string argument) {
    string *names = ({});
    foreach (object guest : users()) {
        string guest_name = guest->query_name();
        if (guest_name) {
            names += ({ capitalize(guest_name) });
        }
    }
    write("Online: " + implode(sort_array(names), ", ") + ".\n");
    return 1;
}

/**
 * Shared helper for explicit quit and the net_dead() disconnect apply.
 * No arguments or return value; announce only a named guest who entered a
 * room, then destruct this body, which the master permits for self-cleanup.
 * No name registry needs updating: later claims scan the connected bodies.
 */
private void leave() {
    if (name && environment()) {
        environment()->announce(capitalize(name) + " has left.\n", this_object());
    }
    destruct(this_object());
}

/**
 * Personal action ending a named guest's connection; argument is ignored.
 * Send the farewell before leave() destructs the body and closes the socket,
 * then return 1 so quit is recorded as a handled command.
 * This intentional destruction does not cause a second net_dead() callback.
 */
private int do_quit(string argument) {
    write("Goodbye, " + capitalize(name) + "!\n");
    leave();
    return 1;
}

/**
 * Clean up after the client disconnects unexpectedly, including idle expiry.
 * The connection is already detached, interactive() is false, and
 * this_player() is this body; there are no arguments or useful return value.
 * leave() also handles an unnamed body that never entered the room.
 * Omitting this hook would leave the disconnected body behind; orderly
 * server shutdown, exec(), failed logon() and destruction do not call it.
 */
void net_dead() {
    leave();
}
