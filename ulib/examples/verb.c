/**
 * Optional grammar handlers for "give OBJ to LIV", disabled as teaching examples.
 * OBJ matches an item and LIV matches a living recipient; "to" is literal.
 * create/can/do belong on the verb object, direct on the item, and indirect
 * on the recipient, so this file must not be installed as one verb object.
 * Name matching also needs vocabulary or id(), as in object.c and ../obj/player.c.
 *
 * In the examples below, item_words and recipient_words are the noun phrases
 * as typed; item and recipient hold resolved objects or 0 while unresolved.
 * The specific handler name wins over its generic fallback when both exist.
 * These samples refuse gifts and do not implement transfer or ownership.
 * See ../doc/applies.md, "Grammar handler families", for other rule shapes.
 */
#pragma strict_types

#if 0
/**
 * Initialise a verb object and register a driver-wide grammar rule.
 * The driver calls create() after global initialisers; its return is ignored.
 * parse_init() is required before parse_add_rule(), whose registration is
 * tried after a living's own actions decline the verb, wherever the verb
 * object is located; parse_sentence() can also invoke the parser explicitly.
 * Omitting this initialiser leaves this example's rule unregistered.
 */
void create() {
    parse_init();
    parse_add_rule("give", "OBJ to LIV");
}

/**
 * On the verb object, decide whether the actor may attempt this rule at all.
 * item and recipient are both 0 here; the two *_words strings are available
 * for checks that do not need resolved objects, and this_player() is the actor.
 * Return 1 to continue, 0 to refuse, or a reason string from a mixed-return
 * version; when neither specific nor generic can hook exists, permission
 * defaults to yes, as this simple example does explicitly.
 */
int can_give_obj_to_liv(object item, object recipient, string item_words, string recipient_words) {
    return 1;
}

/**
 * On an item, decide whether it qualifies as the object being given.
 * item is this object during filtering; recipient may still be 0, while
 * item_words and recipient_words retain the typed noun phrases.
 * The parser asks again with both objects filled after resolving the rule.
 * Return 1 to qualify, 0 to reject, or a reason string from a mixed-return
 * version; without a specific or generic direct hook the item is excluded.
 * Requiring the item to be carried by this_player() permits only held gifts.
 */
int direct_give_obj_to_liv(object item, object recipient, string item_words, string recipient_words) {
    return environment(this_object()) == this_player();
}

/**
 * On a living recipient, decide whether it accepts the item from this_player().
 * item is the selected gift and recipient is this living; the *_words strings
 * hold the typed phrases, and this hook is rechecked after full resolution.
 * Return 1 to accept, 0 to reject silently, or a refusal string as here.
 * The string reaches parser_error_message() as kind 6; a leading # makes
 * that reason yield to a plain reason from another matching object.
 * Omitting both indirect hooks excludes this recipient from the rule.
 */
mixed indirect_give_obj_to_liv(object item, object recipient, string item_words, string recipient_words) {
    return "This example does not accept gifts.";
}

/**
 * On the verb object, perform the action after all permission hooks accept.
 * item and recipient are now resolved objects; the *_words strings preserve
 * the phrases used to select them, and this_player() remains the actor.
 * The return value is ignored: reject an action in can/direct/indirect,
 * because reaching do marks the rule handled even if it returns 0.
 * Omitting both do hooks leaves the rule unhandled; this example only prints
 * a teaching message, and the refusing indirect hook normally prevents it.
 */
void do_give_obj_to_liv(object item, object recipient, string item_words, string recipient_words) {
    write("The grammar matched; implement the transfer here.\n");
}

/**
 * On the verb object, replace a missing can_<verb>_<slug> hook.
 * verb is the base verb, including for synonyms, and rule is its registered
 * text; here they are "give" and "OBJ to LIV", followed by unresolved object
 * slots and their typed phrases, just as for can_give_obj_to_liv().
 * This fallback refuses with 0; omission of both can hooks defaults to yes.
 * The signature fits only rules with these slots; it is not a universal
 * handler for arbitrary rules despite the word "generic".
 */
int can_verb_rule(string verb, string rule, object item, object recipient, string item_words, string recipient_words) {
    return 0;
}

/**
 * On an item, replace a missing direct_<verb>_<slug> hook.
 * verb and rule identify the base verb and registered rule; the remaining
 * arguments have the same filtering/recheck meanings as direct_give_obj_to_liv().
 * Return 1 to qualify or 0 to exclude; use a mixed return to supply a reason.
 * This fallback excludes all items, as omission of both direct hooks would.
 * Its trailing arguments fit OBJ-to-LIV rules only; adapt them for other rules.
 */
int direct_verb_rule(string verb, string rule, object item, object recipient, string item_words, string recipient_words) {
    return 0;
}

/**
 * On a recipient, replace a missing indirect_<verb>_<slug> hook.
 * verb and rule identify the base verb and registered rule; item, recipient
 * and both phrases have the meanings shown in indirect_give_obj_to_liv().
 * Return 1 to accept or 0 to exclude; use a mixed return to supply a reason.
 * This fallback refuses, as omission of both indirect hooks would.
 * Its trailing arguments fit OBJ-to-LIV rules only; adapt them for other rules.
 */
int indirect_verb_rule(string verb, string rule, object item, object recipient, string item_words, string recipient_words) {
    return 0;
}

/**
 * On the verb object, replace a missing do_<verb>_<slug> action hook.
 * verb and rule identify the base verb and registered rule; resolved item,
 * recipient and their typed phrases follow, as in do_give_obj_to_liv().
 * Its trailing arguments fit OBJ-to-LIV rules only; adapt them for other rules.
 * The return is ignored: this empty example would count as handled if reached,
 * whereas omission of both do hooks leaves the rule unhandled.
 * Keep unsupported rules rejected by the permission hooks above.
 */
void do_verb_rule(string verb, string rule, object item, object recipient, string item_words, string recipient_words) {
}
#endif
