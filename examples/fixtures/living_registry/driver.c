private object *readers;
private string *reader_names;
private object *writers;

void populate(int count, int workers) {
    readers = allocate(count);
    reader_names = allocate(count);
    writers = allocate(workers);
    for (int i = 0; i < count; i++) {
        readers[i] = clone_object("/actor");
        reader_names[i] = "reader" + i;
        seed_living(readers[i], reader_names[i]);
    }
    for (int i = 0; i < workers; i++) {
        writers[i] = clone_object("/actor");
        writers[i]->prepare(i);
    }
}

int lookup(int index, int work) {
    object found = find_living(reader_names[index]);
    int sum = 0;
    for (int i = 0; i < work; i++) {
        sum += i;
    }
    return found == readers[index] && sum == work * (work - 1) / 2;
}

int rename(int index) {
    return writers[index]->rename();
}

int verify() {
    int total = 0;
    for (int i = 0; i < sizeof(readers); i++) {
        if (find_living(reader_names[i]) != readers[i]) {
            throw("lookup lost a registered object");
        }
    }
    foreach (object writer : writers) {
        total += writer->verify();
    }
    return total;
}
