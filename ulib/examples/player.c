/**
 * Optional applies for the body currently attached to a connection.
 * The master's connect() returns the initial body; exec() may replace it.
 * ulib uses /obj/player.c for that role; its filename is a mudlib choice.
 * These examples remain disabled until needed.
 * The driver already negotiates the protocols; these hooks give the mudlib
 * a place to respond to client events without adding ordinary chat commands.
 * Both examples intentionally do nothing; simple line-based chat needs no
 * client metadata or terminal layout state.
 * See ../doc/applies.md, "Special applies", for the complete hook index.
 */
#pragma strict_types

#if 0
/**
 * Receive a Generic MUD Communication Protocol message on the connected body.
 * package names the message, such as "Core.Hello"; payload is the following
 * text, usually JSON, or an empty string when only the name was sent.
 * this_player() is the body, even while logon() or input_to() is pending.
 * Omission ignores these messages; the return value is not used.
 * Before adding behaviour, check query_name() if it requires completed login,
 * select supported packages, then validate any json_decode() result.
 * Client-supplied metadata is not proof of a player's identity.
 */
void gmcp(string package, string payload) {
}

/**
 * Receive terminal dimensions reported by Telnet NAWS (window-size negotiation).
 * cols and rows are the reported column and row counts; this_player() is the
 * connected body, and query_connection(this_object()) already stores them.
 * The hook also runs when a body is attached if dimensions are already known;
 * clients that never report dimensions never trigger it.
 * Omission performs no mudlib layout work and the return value is ignored.
 * A future screen interface could redraw here; plain chat needs no redraw.
 */
void window_size(int cols, int rows) {
}
#endif
