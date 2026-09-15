inherit "/base";
int left = 2;

int read_cells() {
    int sum;
    for (int i = 0; i < 20000; i++) {
        sum += base;
        sum += left;
    }
    return sum;
}

int references() {
    for (int i = 0; i < 2000; i++) {
        increment(ref base);
        increment(ref left);
    }
    return left;
}

int tick() { return base; }

int calls() {
    int sum;
    for (int i = 0; i < 2000; i++) sum += tick();
    return sum;
}

function callback() {
    int captured = 1;
    return (: base + left + captured :);
}
