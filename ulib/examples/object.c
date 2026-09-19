/**
 * Optional applies for an object representing a small wooden box.
 * Copy these hooks into a new game object when adding noun-based commands.
 * This disabled example supplies vocabulary and parser access, not a complete
 * container: it has no open/close state or item-transfer commands.
 * See ../doc/applies.md, "Object applies", for the complete hook index.
 */
#pragma strict_types

#if 0
/**
 * Supply this object's singular names for grammar noun matching.
 * Return a string array of alternatives; "box" combines with the adjectives
 * below so both "box" and "small wooden box" can identify this object.
 * Without this hook the parser tries id(phrase); defining it disables that
 * fallback even if it returns no array, so provide all intended names here.
 */
string *parse_command_id_list() {
    return ({ "box" });
}

/**
 * Supply this object's plural names for phrases such as "two wooden boxes".
 * Return a string array; omission or a non-array result asks the master's
 * parse_command_pluralize() to derive names from the singular-id list.
 * Supplying "boxes" here avoids the master example's simplistic "boxs".
 */
string *parse_command_plural_id_list() {
    return ({ "boxes" });
}

/**
 * Supply words that may precede this object's singular or plural names.
 * Return a string array; these entries allow "wooden box" and "small box"
 * without making "wooden" a noun or changing what a look command displays.
 * Omission supplies no object-specific adjectives; shared master adjectives
 * are separate, and "adjectiv" is the required spelling of this apply.
 */
string *parse_command_adjectiv_id_list() {
    return ({ "wooden", "small" });
}

/**
 * Decide whether the grammar parser searches this object's inventory.
 * Return 1, as here, to include its contents, or 0 to hide them from search.
 * Omission defaults to visible; a real closed opaque box would return 0
 * according to its open/closed state rather than use this fixed answer.
 * This controls finding contents, not whether an action may reach them.
 */
int inventory_visible() {
    return 1;
}

/**
 * Decide whether contents found by the grammar parser can be reached.
 * Return 1 to allow access or 0 to make matches inaccessible; omission
 * defaults to accessible, as does this open-box example.
 * A closed glass case could be visible but inaccessible, producing a
 * "cannot reach" failure instead of pretending its contents do not exist.
 * This is a parser rule, not permission to bypass the master's security hooks.
 */
int inventory_accessible() {
    return 1;
}
#endif
