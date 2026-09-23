/**
 * Optional object and text grammar handlers, disabled as teaching examples.
 * OBJ matches an item and LIV matches a living recipient; "to" is literal.
 * create/do belong on the verb object, direct on the item, and indirect
 * on the recipient; can may live on the actor or verb object.
 * This file must not be installed as one verb object.
 * Name matching also needs vocabulary or id(), as in object.c and ../obj/player.c.
 *
 * In the examples below, item_words and recipient_words are the noun phrases
 * as typed; can receives nil objects, while pair permissions receive both objects.
 * Nil is 0 in the default mode; distinct mode keeps it separate from numeric 0.
 * Each target tries specific, literal-word, generic-shape, then generic-rule
 * handlers. Only absence tries another tier or target. Can searches the actor
 * before the owner; direct/indirect search the noun candidate before the owner.
 * A present actor can supplies the answer instead of the owner can.
 * Longer matching verb phrases run first, with registration order within each phrase.
 * Refusals leave later rules and shorter phrases eligible; the first do wins.
 * Error reporting waits until all phrases fail, and refusals do not undo effects.
 * "hand over" below uses give's methods; query_verb() reports "hand over".
 * query_verb(1) and generic typed-verb arguments preserve the whole typed phrase.
 * Whitespace in registered keys is normalized; case and punctuation stay exact.
 * Direct multiword bases use generic methods, without invented underscore names.
 * These samples refuse gifts and do not implement transfer or ownership.
 * Their unmodified object tokens require reachable matches. For an observation
 * verb, parse_add_rule("look", "at OBJ:v") permits visible but unreachable
 * objects, still using can_look_at_obj/direct_look_at_obj/do_look_at_obj.
 * :v also works on OBS, LIV and LVS; it changes no handler argument layout.
 * Generic rule arguments preserve the modifier exactly as registered.
 * A supplied scope like ({ box, ({ item }) }) applies box's inventory hooks;
 * ({ item }) instead explicitly makes item reachable.
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
    parse_add_synonym("hand over", "give");
    parse_add_rule("cast", "STR at STR");
}

/**
 * On the actor, or the verb object if absent there, permit an attempt.
 * item and recipient are both 0 here; the two *_words strings are available
 * for checks that do not need resolved objects, and this_player() is the actor.
 * Return 1 to continue, 0 to refuse, or a reason string from a mixed-return
 * version; when no can tier exists on either target, permission defaults
 * to yes, as this simple example does explicitly.
 */
int can_give_obj_to_liv(object item, object recipient, string item_words, string recipient_words) {
    return 1;
}

/**
 * On an item, decide whether it qualifies as the object being given.
 * item is this object and recipient is the living being checked with it;
 * item_words and recipient_words retain the typed noun phrases.
 * Each possible pair is checked before selection, then selected pairs are rechecked.
 * Return 1 to qualify, 0 to reject, or a reason string from a mixed-return
 * version; without any direct tier on this item or the owner, it is excluded.
 * Requiring the item to be carried by this_player() permits only held gifts.
 */
int direct_give_obj_to_liv(object item, object recipient, string item_words, string recipient_words) {
    return environment(this_object()) == this_player();
}

/**
 * On a living recipient, decide whether it accepts the item from this_player().
 * item is the gift being checked and recipient is this living; the *_words
 * strings hold the typed phrases, and selected pairs are rechecked afterward.
 * This hook runs only if the pair's direct check accepts and both objects survive.
 * Return 1 to accept, 0 to reject silently, or a refusal string as here.
 * If all rules fail, this reason can reach parser_error_message() as kind 6.
 * A leading # makes it yield to a plain reason from another matching object.
 * Omitting all indirect tiers on this recipient and the owner excludes it.
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
 * Omitting all do tiers leaves the rule unhandled; this example only prints
 * a teaching message, and the refusing indirect hook normally prevents it.
 */
void do_give_obj_to_liv(object item, object recipient, string item_words, string recipient_words) {
    write("The grammar matched; implement the transfer here.\n");
}

/**
 * On the actor or owner, answer after its other three can tiers are absent.
 * verb is the base verb, including for synonyms, and rule is its registered
 * text; here they are "give" and "OBJ to LIV", followed by unresolved object
 * slots, the typed verb (possibly a synonym), and their typed phrases.
 * This fallback refuses with 0; omission of all can tiers defaults to yes.
 * The signature fits only rules with these slots; it is not a universal
 * handler for arbitrary rules despite the word "generic".
 */
int can_verb_rule(string verb, string rule, object item, object recipient, string typed_verb, string item_words, string recipient_words) {
    return 0;
}

/**
 * On an item, answer after its other three direct tiers are absent.
 * verb and rule identify the base verb and registered rule; the remaining
 * object arguments have the same filtering/recheck meanings as the specific hook;
 * typed_verb precedes the two source phrases.
 * Return 1 to qualify or 0 to exclude; use a mixed return to supply a reason.
 * This fallback excludes all items; its refusal does not try the owner.
 * Its trailing arguments fit OBJ-to-LIV rules only; adapt them for other rules.
 */
int direct_verb_rule(string verb, string rule, object item, object recipient, string typed_verb, string item_words, string recipient_words) {
    return 0;
}

/**
 * On a recipient, answer after its other three indirect tiers are absent.
 * verb and rule identify the base verb and registered rule; item, recipient
 * and both phrases have the meanings shown in the specific hook, with
 * typed_verb before the two phrases.
 * Return 1 to accept or 0 to exclude; use a mixed return to supply a reason.
 * This fallback refuses; its refusal does not try the owner.
 * Its trailing arguments fit OBJ-to-LIV rules only; adapt them for other rules.
 */
int indirect_verb_rule(string verb, string rule, object item, object recipient, string typed_verb, string item_words, string recipient_words) {
    return 0;
}

/**
 * On the verb object, act after its other three do tiers are absent.
 * verb and rule identify the base verb and registered rule; resolved item,
 * recipient, the typed verb, and their typed phrases follow.
 * Its trailing arguments fit OBJ-to-LIV rules only; adapt them for other rules.
 * The return is ignored: this empty example would count as handled if reached,
 * whereas omission of all do tiers leaves the rule unhandled.
 * Keep unsupported rules rejected by the permission hooks above.
 */
void do_verb_rule(string verb, string rule, object item, object recipient, string typed_verb, string item_words, string recipient_words) {
}

/**
 * On the actor or verb object, handle the literal-word tier of this rule.
 * The literal "to" becomes an argument between the two unresolved captures.
 * The specific can_give_obj_to_liv name takes precedence when present.
 */
int can_give_obj_word_liv(object item, string prep, object recipient, string item_words, string recipient_words) {
    return prep == "to";
}

/**
 * On the verb object, demonstrate the generic-shape action for this rule.
 * The base verb precedes capture/literal values; the typed verb precedes
 * source phrases. Both verb-specific action tiers take precedence.
 * This example prints a teaching message after the permission checks succeed.
 */
void do_verb_obj_word_liv(string verb, object item, string prep, object recipient, string typed_verb, string item_words, string recipient_words) {
    write("The grammar matched; implement the transfer here.\n");
}

/**
 * On the actor or verb object, require a supported spell for this text split.
 * Both captures are nonempty; raw strings repeat their semantic text values.
 * Longer first captures are tried first, so refusing "spark at dusk" allows
 * a later split with spell "spark" and target "dusk at tower".
 * These are strings; this rule calls no direct or indirect object permissions.
 */
int can_cast_str_at_str(string spell, string target, string spell_words, string target_words) {
    return spell == "spark";
}

/**
 * On the verb object, demonstrate the accepted pair of text captures.
 * No spell lookup or target-object resolution is implied by this teaching message.
 */
void do_cast_str_at_str(string spell, string target, string spell_words, string target_words) {
    write("The grammar matched " + spell + " at " + target + ".\n");
}
#endif
