/**
 * The starting room preloaded by the master and entered after name selection.
 * Inheriting /std/room supplies its descriptions, chat actions and cleanup;
 * another room can use the same pattern with its own title and long description.
 */
#pragma strict_types
inherit "/std/room";

/**
 * Set this room's identity when first loaded, with no arguments or useful return.
 * ::create() explicitly runs the inherited initialiser before these overrides;
 * its defaults would otherwise be skipped by defining our own create().
 * The inherited protected setters configure the text read by the look action,
 * while inherited init() and clean_up() continue to work without overrides.
 */
void create() {
    ::create();
    set_short("The Common Room");
    set_long("A small, quiet room with enough chairs for everyone.\n"
        "There is no quest here: stay awhile and talk.\n");
}
