/**
 * Optional master-object applies; none is needed by the chat world.
 * The driver calls these on the object selected by LPC_MASTER_OBJECT.
 * ulib selects /secure/master.c; another mudlib may configure a different path.
 * Copy an individual hook into the live master when adding its feature.
 * The #if 0 block keeps these examples out of the compiled program.
 *
 * The vocabulary hooks serve parse_command() and the grammar parser used by
 * parse_sentence(); they do not change ulib's add_action() commands.
 * See ../doc/applies.md, "Master applies", for the complete hook index.
 */
#pragma strict_types

#if 0
/**
 * Resolve a requested object whose source file does not exist.
 * path is its absolute in-game name without .c; func is the loading efun or
 * "call_other", caller requested the load, and program defines that code.
 * Return an existing blueprint's path to run its code under the requested
 * name, with separate globals; the blueprint must permit cloning.
 * Returning 0, as here, or omitting the hook leaves the ordinary load to fail.
 * The hook runs before valid_load(), which checks a non-resident blueprint.
 */
mixed compile_object(string path, string func, object caller, string program) {
    return 0;
}

/**
 * Rewrite a command before the player's process_input() and command handlers.
 * line is the input command and player is the living issuing it.
 * Return the original string to continue unchanged, or a replacement string
 * for a global alias; a non-string result consumes the line entirely.
 * Missing this hook also passes the original line through.
 * Returning 0 here would swallow commands, unlike process_input()'s 0.
 */
string modify_command(string line, object player) {
    return line;
}

/**
 * Supply shared singular names for objects using list-based noun matching.
 * This array adds "thing" to each object's own names; it creates no object
 * and does not make an ambiguous reference choose one object automatically.
 * Omission supplies no shared names; object-specific names belong in that
 * object's parse_command_id_list(), illustrated in object.c.
 * Objects using the id() fallback must match phrases themselves instead.
 */
string *parse_command_id_list() {
    return ({ "thing" });
}

/**
 * Supply plural names shared by all objects, such as "things" or "them".
 * Return a string array; omission supplies no shared plural names.
 * These names let plural noun phrases match groups; an individual object's
 * irregular plural, such as "boxes", belongs in its own hook instead.
 */
string *parse_command_plural_id_list() {
    return ({ "things" });
}

/**
 * Supply adjectives accepted before every object's names, such as "that".
 * An empty array, as here, or an omitted hook adds no shared adjectives.
 * Add descriptive words such as "wooden" on the particular object instead.
 * The spelling "adjectiv" is part of the apply name and must be preserved.
 */
string *parse_command_adjectiv_id_list() {
    return ({});
}

/**
 * Supply parse_command()'s default vocabulary for a %p preposition match.
 * It is used when the caller's destination does not already hold an array.
 * Return a string array; an entry can be a phrase such as "in front of".
 * Omission supplies no default list; these words do not register commands.
 */
string *parse_command_prepos_list() {
    return ({ "in", "on", "to", "from", "with" });
}

/**
 * Supply the noun-phrase word meaning every matching object, as in "all boxes".
 * Return one string; omission leaves the parser without an all-word.
 * Matching still depends on object names, scope and the rule's slot type.
 */
string parse_command_all_word() {
    return "all";
}

/**
 * Interpret the first word of a noun phrase when it is not digits or "all".
 * word is that token; return a positive count for "two boxes", a negative
 * ordinal for "second box", or 0 if the word is not a recognised number.
 * This example recognises only one, two, first and second; omission treats
 * no words as numerals, without disabling the parser's handling of digits.
 */
int parse_command_numeral(string word) {
    switch (word) {
        case "one": return 1;
        case "two": return 2;
        case "first": return -1;
        case "second": return -2;
    }
    return 0;
}

/**
 * Derive plurals when an object's plural-id hook supplies no array.
 * singulars contains that object's singular ids; return the plural for each
 * singular at the same index, or a non-string entry to supply no plural.
 * Omission derives nothing; this minimal English example only appends "s".
 * Give irregular words their own plural-id hook: "box" must become "boxes".
 */
string *parse_command_pluralize(string *singulars) {
    string *plurals = ({});
    foreach (string singular : singulars) {
        plurals += ({ singular + "s" });
    }
    return plurals;
}

/**
 * Add named guests beyond the actor's local scope to LIV/LVS grammar slots.
 * Return an object array; only objects that called enable_commands() qualify,
 * and these extra objects are never offered to OBJ/OBS slots.
 * This example relies on ulib's query_name() helper to omit unfinished logins.
 * Omission adds nobody beyond local scope; enabling this hook permits names
 * to resolve across rooms, so action handlers must check any distance rules.
 */
object *parse_command_users() {
    object *guests = ({});
    foreach (object guest : users()) {
        if (guest->query_name()) {
            guests += ({ guest });
        }
    }
    return guests;
}

/**
 * Turn a grammar failure into text for parse_sentence() or command dispatch.
 * kind selects the failure: 2 non-living, 3 inaccessible, 4 ambiguous,
 * 5 ordinal out of range, 6 handler refusal, 7 unknown phrase, 8 bad plural.
 * arg is the phrase for 2/3/7, objects for 4, a count for 5, a reason for 6,
 * or 0 for 8; ob is the refusing object for kind 6 and otherwise 0.
 * flag marks a plural slot for 2/3/7 and is otherwise 0.
 * Return a message, or 0 for no parser message; omission also supplies none.
 * Dispatch may then try other rules or command_not_found().
 * This example preserves a handler's reason and uses one fallback otherwise.
 */
string parser_error_message(int kind, object ob, mixed arg, int flag) {
    if (kind == 6 && stringp(arg)) {
        return arg + "\n";
    }
    return "That command did not match an available action.\n";
}
#endif
