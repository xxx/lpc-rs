void inc(int ref x) { x++; }
int run() { int y = 1; inc(ref y); return y; }
