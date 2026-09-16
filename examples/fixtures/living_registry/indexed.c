#ifndef SHARDS
#define SHARDS 1
#endif

private mapping *names;
private mapping *old_names;
private object *players = ({ });

int pointerp(mixed arg) {
    return arrayp(arg);
}

void create() {
    names = allocate(SHARDS);
    old_names = allocate(SHARDS);
    for (int i = 0; i < SHARDS; i++) {
        names[i] = ([ ]);
        old_names[i] = ([ ]);
    }
}

private int bucket(string key) {
#if SHARDS == 1
    return 0;
#else
    int hash = 0;
    for (int i = 0; i < sizeof(key); i++) {
        hash = (hash * 31 + key[i]) % SHARDS;
    }
    return hash;
#endif
}

void seed_living(object ob, string name) {
    names[bucket(name)][name] = ob ? ({ ob }) : ({ });
    if (ob) {
        old_names[bucket(file_name(ob))][ob] = name;
    }
}

void set_living_name(string name) {
    object ob = previous_object();
    mapping reverse = old_names[bucket(file_name(ob))];
    string old = reverse[ob];
    mapping target;
    mixed list;

    if (interactive(ob)) {
        players = (players - ({ 0, ob })) + ({ ob });
    }
    if (stringp(old)) {
        mapping previous = names[bucket(old)];
        previous[old] = previous[old] - ({ ob });
    }
    target = names[bucket(name)];
    list = target[name];
    if (!pointerp(list)) {
        list = ({ });
    }
    target[name] = (list - ({ 0, ob })) + ({ ob });
    reverse[ob] = name;
}

private object *living_named(string name) {
    mixed list = names[bucket(name)][name];
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
