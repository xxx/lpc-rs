/**
 * Base room: descriptions, local chat actions and disposal when empty and idle.
 * A concrete room inherits /std/room, calls ::create() and sets its descriptions,
 * as ../room/lounge.c does; the room's inventory holds its occupants.
 *
 * create(), init() and clean_up() are driver applies; the other functions are
 * mudlib helpers or handlers registered with add_action().
 * See ../doc/add-a-command.md to add another room action.
 */
#pragma strict_types
#include <ulib.h>

private string short_description;
private string long_description;

/**
 * Initialise a new room after its global variables have been initialised.
 * No arguments or useful return value; these defaults let a derived room
 * override either description without leaving the other as an unset string.
 * A derived create() must call ::create() explicitly to run this parent hook;
 * omitting the hook entirely leaves only the global initialisers in effect.
 */
void create() {
    short_description = "A room";
    long_description = "An unfinished room.\n";
}

/**
 * Set the room title from a derived room; no return value.
 * description is plain text without a required trailing newline, since look
 * adds its own separation; protected keeps this setter off the public -> API.
 */
protected void set_short(string description) {
    short_description = description;
}

/**
 * Set the room's main description from a derived room; no return value.
 * Include a final newline in description so the player's prompt starts on
 * its own line; protected limits this setter to the room and its inheritors.
 */
protected void set_long(string description) {
    long_description = description;
}

/**
 * Offer local commands when a living enters this room's presence.
 * this_player() and previous_object() identify the living receiving actions;
 * there are no explicit arguments and the return value is ignored.
 * add_action() registers handlers on this room for that living, and movement
 * away removes them; omitting this hook offers none of these local verbs.
 */
void init() {
    add_action("do_look", "look");
    add_action("do_say", "say");
    add_action("do_emote", "emote");
}

/**
 * Room action for look; argument is the command remainder or 0 and is ignored.
 * write() addresses this_player(), so the arriving guest sees the room's
 * description even though the executing object is the room.
 * Return 1 to consume the command; object-specific "look at" is not implemented.
 */
private int do_look(string argument) {
    write(short_description + "\n\n" + long_description);
    return 1;
}

/**
 * Mudlib helper sending an already formatted message to connected occupants.
 * message should include any desired newline; exclude is an object to skip,
 * or 0 to include everyone; there is no return value.
 * Name and connection checks exclude unfinished or disconnected players,
 * and tell_object() reaches each recipient's catch_tell() delivery hook.
 * Callers validate user text before passing it here, as the chat actions do.
 */
void announce(string message, object exclude) {
    foreach (object guest : all_inventory(this_object())) {
        if (guest != exclude && interactive(guest) && guest->query_name()) {
            tell_object(guest, message);
        }
    }
}

/**
 * Shared validation for the text after say or emote; it may be 0 when absent.
 * Return 1 for usable chat or 0 after explaining the problem to this_player().
 * sizeof() measures string characters; the regexes require non-whitespace
 * and reject Unicode Cc controls, Zl line separators and Zp paragraph separators.
 * This keeps chat within the configured limit and prevents terminal escape
 * sequences; it neither evaluates LPC nor interprets colour or markup tokens.
 */
private int valid_text(string text) {
    if (!text || !sizeof(text) || !regmatch(text, "\\S")) {
        write("Please supply some text.\n");
        return 0;
    }
    if (sizeof(text) > MAX_CHAT_LENGTH) {
        write("Keep messages to " + MAX_CHAT_LENGTH + " characters or fewer.\n");
        return 0;
    }
    if (regmatch(text, "[\\p{Cc}\\p{Zl}\\p{Zp}]")) {
        write("Please use a single line of text without control characters.\n");
        return 0;
    }
    return 1;
}

/**
 * Room action for speech; text is the command remainder or 0 if absent.
 * A valid line gives the speaker "You say" and other occupants "Name says";
 * excluding the speaker from announce() prevents duplicate delivery.
 * Return 1 even after validation fails, because its explanatory message has
 * already handled the command and command_not_found() must not run as well.
 */
private int do_say(string text) {
    if (valid_text(text)) {
        object speaker = this_player();
        write("You say: " + text + "\n");
        announce(capitalize(speaker->query_name()) + " says: " + text + "\n", speaker);
    }
    return 1;
}

/**
 * Room action for an action description; text is the remainder or 0 if absent.
 * Valid text is prefixed with the player's name and announced to everyone,
 * including its author, so "emote waves." produces "Alice waves." for all.
 * Return 1 on both success and a reported validation failure to consume it.
 */
private int do_emote(string text) {
    if (valid_text(text)) {
        announce(capitalize(this_player()->query_name()) + " " + text + "\n", 0);
    }
    return 1;
}

/**
 * Consider disposing of this room when the driver finds it idle.
 * references is 0 for a clone, or 1 plus the live clones of a prototype;
 * it does not count arbitrary references or objects inheriting this room.
 * Return 1 to allow a later check when occupants or clones still need it.
 * Otherwise destruct explicitly: returning 0 only stops future cleanup calls.
 * Omission provides no automatic room disposal; this policy is suitable only
 * for disposable state, so add checks before storing belongings or events here.
 */
int clean_up(int references) {
    if (references > 1 || sizeof(all_inventory(this_object()))) {
        return 1;
    }
    destruct(this_object());
    return 0;
}
