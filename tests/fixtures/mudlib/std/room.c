private string short_desc;
private string long_desc;

nomask public void create() {
    short_desc = "A room";
    long_desc = "A room.\n";
    this_object()->create_room();
}

public void create_room() {}

public string query_short() {
    return short_desc;
}

public string query_long() {
    return long_desc;
}

public void set_short(string desc) {
    short_desc = desc;
}

public void set_long(string desc) {
    long_desc = desc;
}
