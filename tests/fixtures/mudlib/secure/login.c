#pragma strict_types

string name;

// The driver applies logon on the object connect() returned; returning 1
// keeps the connection.
int logon(string ip, int port) {
    write("lpc-rs smoke lib. You are " + ip + ":" + port + ".\n");
    write("What is your name? ");
    input_to(get_name);
    return 1;
}

private void get_name(string input) {
    name = input;
    write("Welcome, " + name + "!\n");
    enable_commands();
    add_action("do_look", "look");
    add_action("do_stats", "stats");
    add_action("do_gmcp", "gmcp");
    add_action("do_mxp", "mxp");
    add_action("do_quit", "quit");
    move_object("/area/start/spark");
    do_look("");
}

string write_prompt() {
    return name ? name + "> " : "> ";
}

void catch_tell(string message) {
    write_socket(message);
}

// The driver applies these when the client sends a GMCP message or reports
// its window size; echoing them makes both visible to the test.
void gmcp(string package, string payload) {
    write("[gmcp] " + package + " " + payload + "\n");
}

void window_size(int cols, int rows) {
    write("[naws] " + cols + "x" + rows + "\n");
}

private int do_look(string arg) {
    object env = environment(this_player());
    if (!env) {
        write("You float in the void.\n");
        return 1;
    }
    write(env->query_short() + "\n" + env->query_long());
    return 1;
}

private int do_stats(string arg) {
    mapping m = query_connection();
    write("cols: " + m["cols"] + "\n");
    write("rows: " + m["rows"] + "\n");
    write("gmcp: " + m["gmcp"] + "\n");
    write("mxp: " + m["mxp"] + "\n");
    write("eor: " + m["eor"] + "\n");
    return 1;
}

private int do_gmcp(string arg) {
    send_gmcp(this_player(), "Smoke.Echo", "{ \"who\": \"" + name + "\" }");
    write("gmcp sent.\n");
    return 1;
}

private int do_mxp(string arg) {
    send_mxp(this_player(), "<b>bold</b>\n");
    write("mxp sent.\n");
    return 1;
}

private int do_quit(string arg) {
    write("Goodbye, " + name + "!\n");
    destruct(this_player());
    return 1;
}
