// Registration and lookup from cdlib-lpc/secure/lpc-rs.c.
private mapping living_names = ([ ]);
private object *players = ({ });

int pointerp(mixed arg) {
    return arrayp(arg);
}

void seed_living(object ob, string name) {
    living_names[name] = ob ? ({ ob }) : ({ });
}

void set_living_name(string name) {
    object ob = previous_object();
    mixed list;

    if (interactive(ob)) {
        players = (players - ({ 0, ob })) + ({ ob });
    }
    foreach (string other, mixed each : living_names) {
        if (pointerp(each)) {
            living_names[other] = each - ({ ob });
        }
    }
    list = living_names[name];
    if (!pointerp(list)) {
        list = ({ });
    }
    living_names[name] = (list - ({ 0, ob })) + ({ ob });
}

private object *living_named(string name) {
    mixed list = living_names[name];
    if (!pointerp(list)) {
        return ({ });
    }
    return filter(list, (: objectp($1) && living($1) :));
}

varargs mixed find_living(string name, int all) {
    object *found = living_named(name);
    if (all) {
        return found;
    }
    return sizeof(found) ? found[0] : 0;
}
