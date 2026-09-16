private string first;
private string second;
private int changes;

void create() {
    enable_commands();
}

void prepare(int id) {
    first = "writer" + id + "a";
    second = "writer" + id + "b";
    seed_living(this_object(), first);
    seed_living(0, second);
}

int rename() {
    set_living_name(changes % 2 ? first : second);
    changes++;
    return changes;
}

int verify() {
    string current = changes % 2 ? second : first;
    string old = changes % 2 ? first : second;
    if (find_living(current) != this_object() || find_living(old)) {
        throw("incorrect rename result");
    }
    return changes;
}
