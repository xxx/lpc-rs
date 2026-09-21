/**
 * The master is the driver's entry point for startup, login and permissions.
 * It is loaded from LPC_MASTER_OBJECT before ordinary game objects.
 * no_clone, no_inherit and no_shadow keep this policy object from being
 * duplicated, inherited or intercepted by another object's shadow functions.
 *
 * Policy hooks return 1 to allow or 0 to deny; omission also denies.
 * Their program argument names the source defining the requesting code,
 * which may differ from caller's filename through inheritance, or be 0
 * when an efun pointer has no defining program.
 * All source in the allowed game directories is trusted operator-written code.
 * See ../doc/applies.md and ../doc/how-it-works.md for the wider lifecycle.
 */
#pragma strict_types, no_clone, no_inherit, no_shadow
#include <ulib.h>

/**
 * Choose the body for a new network connection before a player is attached.
 * remote_ip_addr and remote_port identify the client's address and source
 * port; ulib does not use them for name selection or access restrictions.
 * Return a fresh player clone so each connection has its own name and state;
 * the driver attaches the socket and then calls that object's logon().
 * There is no this_player() yet; missing this hook prevents usable login.
 * A mixed-return version may instead return rejection text to disconnect.
 */
object connect(string remote_ip_addr, int remote_port) {
    return clone_object(PLAYER_OBJECT);
}

/**
 * Supply the list of objects to prepare after the master boots, before listening.
 * load_empty is currently always 0; return an array of in-game object paths.
 * The driver passes each path to preload(), rather than loading it itself.
 * Preloading the lounge exposes its compilation errors before guests arrive;
 * omission or a 0 result supplies no preload list.
 */
string *epilog(int load_empty) {
    return ({ START_ROOM });
}

/**
 * Load one in-game path from epilog()'s array; the return value is ignored.
 * load_object() compiles and initialises a missing object, or reuses it if
 * already loaded, and the master's valid_load() policy still applies.
 * Turn a 0 result into a diagnostic so a denied preload is not silent.
 * Omitting this hook leaves the listed objects unloaded by the preload pass.
 */
void preload(string file) {
    if (!load_object(file)) {
        throw("Could not preload " + file);
    }
}

/**
 * Authorise compiling source on an LPC caller's behalf.
 * path is its canonical in-game filename including .c; func names the efun
 * or "call_other", caller requested it, and program defines that code.
 * Allow only game source directories; ulib's choice depends on the path,
 * not which trusted object requested it, and all other paths are denied.
 * This is not a call permission: resident objects and clones of resident
 * prototypes are reused without another valid_load() check.
 */
int valid_load(string path, string func, object caller, string program) {
    return path[0..4] == "/obj/"
        || path[0..5] == "/room/"
        || path[0..4] == "/std/";
}

/**
 * Authorise an explicit inherit while compiling an LPC-loaded object.
 * path is the parent's canonical source filename; from is the source gaining
 * its code, even when the inherit directive came from a header.
 * Allow reusable /std/ code, deny other parents; inherited functions carry
 * their defining file's authority, so privileged policy must stay out of /std/.
 * Boot compilation and a configured auto-inherit file bypass this hook.
 */
int valid_inherit(string path, string from) {
    return path[0..4] == "/std/";
}

/**
 * Authorise an ordinary file read or a compilation include.
 * path is the canonical in-game filename, func names the operation, caller
 * requested it, and program identifies the source defining the calling code.
 * Includes instead use func="include", caller=0 and the including source
 * as program; allow those only under /include/ so ulib.h can be compiled.
 * All ordinary reads are denied, even reads of an otherwise allowed header.
 */
int valid_read(string path, string func, object caller, string program) {
    return func == "include" && path[0..8] == "/include/";
}

/**
 * Authorise filesystem changes and source supplied through compile_string().
 * path is the canonical in-game target, func names the operation, caller
 * requested it, and program defines the requesting code.
 * Return 0 for every request: guest names and conversations are not saved,
 * and loading game code must not also grant permission to supply new code.
 * The same deny-by-default policy applies when this hook is omitted.
 */
int valid_write(string path, string func, object caller, string program) {
    return 0;
}

/**
 * Authorise caller's request to destroy target; program defines the request.
 * Return 1 only for self-destruction, which supports the player's quit and
 * net_dead() paths and the room's clean_up() without letting them delete peers.
 * Omission denies even self-destruction, so removing this hook breaks cleanup.
 */
int valid_destruct(object caller, object target, string program) {
    return caller == target;
}

/**
 * Authorise exec(new_body, old_body), which transfers a live connection.
 * program identifies the requesting code with .c but without a leading slash;
 * new_body receives the connection and old_body currently holds it.
 * Return 0 so exec() refuses; ulib keeps the body originally returned by
 * connect(), whose initial attachment needs no valid_exec() permission.
 */
int valid_exec(string program, object new_body, object old_body) {
    return 0;
}

/**
 * Authorise caller inspecting target's globals with variable_info(target).
 * program is the source defining the inspection request; the operation can
 * expose private values and shared arrays, mappings or callable pointers.
 * Return 0 because ulib needs no inspection of another object's state;
 * self-inspection is allowed by the driver without asking this hook.
 */
int valid_variable_info(object caller, object target, string program) {
    return 0;
}

/**
 * Authorise an in-place upgrade of prototype and its current clones.
 * caller requested it and program defines the request; the active master
 * asks again before compilation, preserving the original command giver.
 * Missing this hook also denies upgrades; return 0 because ulib provides
 * no privileged administration command for upgrading objects.
 * This hook also controls master and simul-efun upgrades; preparation uses
 * the old policy code and global layouts until the entire update commits.
 * Other objects need this grant to query a job's status, using the querying
 * caller, program and command giver; a missing target is passed as 0.
 * The original requester can read its own status without this hook.
 */
int valid_recompile(object prototype, object caller, string program) {
    return 0;
}

/**
 * Authorise the LPC shutdown efun for caller, whose code is defined by program.
 * Return 0 to refuse in-game shutdown; ulib has no privileged administrator.
 * Host signals such as Ctrl-C do not ask this permission and still perform
 * an orderly stop, including the shutdown() notification hook below.
 */
int valid_shutdown(object caller, string program) {
    return 0;
}

/**
 * Authorise intercepting calls to target through another object's shadow.
 * previous_object() is the object asking to become that shadow.
 * Return 0 for all targets because ulib needs no function interception;
 * a target's no_shadow pragma independently forbids shadowing as well.
 */
int query_allow_shadow(object target) {
    return 0;
}

/**
 * Supply feedback when player issues line and no command rule handles it.
 * Return a string for the driver to deliver; 0 would suppress the message,
 * and omission would use the driver's "What?" or unimplemented-living hint.
 * A command's notify_fail() message takes precedence over this hook.
 * A help reminder gives a new guest a way to discover the available verbs.
 */
string command_not_found(object player, string line) {
    return "Unknown command. Type help for the command list.\n";
}

/**
 * Report a failed operation in a fresh transaction after its changes roll back.
 * error contains "error" (message), "location" (source position), "object"
 * (affected object) and "diagnostic" (the rendered source diagnostic).
 * this_interactive() identifies the initiating client's body, which may differ
 * from error["object"]; boot errors and call-outs have no initiating client.
 * The return is ignored; the driver always logs the full error, while this
 * hook gives the guest a short notice; omission also sends the diagnostic.
 */
void error_handler(mapping error) {
    object player = this_interactive();
    if (player) {
        tell_object(player, "Something went wrong; see the driver log.\n");
    }
}

/**
 * Handle a successful compilation's warning in the compiling transaction.
 * warning has "message", "location", "file" and rendered "diagnostic" keys.
 * The return is ignored; omission logs the warning, but defining this hook
 * makes logging our responsibility, so do not silently discard the mapping.
 * With no connection on the master, write_socket() goes to the debug log.
 */
void warning_handler(mapping warning) {
    write_socket(warning["diagnostic"]);
}

/**
 * Describe the server to clients using the MUD Server Status Protocol (MSSP).
 * Return a mapping of protocol field names to values that override defaults;
 * ulib names itself and its game family, with no arguments from the caller.
 * Omission uses driver defaults; custom fields are cached per connection.
 * Leave PLAYERS unset so the driver continues reporting live connection counts.
 */
mapping get_mud_stats() {
    return ([ "NAME": MUD_NAME, "FAMILY": "LPMud" ]);
}

/**
 * Notify clients during the driver's final LPC hook on an orderly shutdown.
 * No arguments or useful return value; missing this hook adds no farewell.
 * Output can still reach connected bodies, including unnamed guests, but
 * new commands and call-outs will not run and net_dead() is not called.
 * ulib has no account or world state to save before the process exits.
 */
void shutdown() {
    foreach (object player : users()) {
        tell_object(player, "The server is shutting down. Goodbye!\n");
    }
}
