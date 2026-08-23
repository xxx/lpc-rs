int secret = 77;
private int hidden() { return 42; }
int sec() { return secret; }
function get_hidden() { return &hidden(); }
mixed fire(function f) { return f(); }
