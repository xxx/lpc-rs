#define HEADER_VALUE 7
// fuzz-source
#include "fuzz.h"
#define TWICE(x) ((x)+(x))
int f() { return TWICE(HEADER_VALUE); }
