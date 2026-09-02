// The master object for the telnet smoke test.

#pragma strict_types, no_clone, no_inherit

object connect() {
    return clone_object("/secure/login");
}

int valid_load(string path, string func, object caller, string program) {
    return 1;
}

int valid_inherit(string path, string from) {
    return 1;
}

void error_handler(mapping error) {
    dump("runtime error:", error, "\n");
}

mapping get_mud_stats() {
    return ([
        "NAME": "lpc-rs smoke",
        "FAMILY": "LPMud",
    ]);
}
