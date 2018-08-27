int add(int x, int y) {
    return x + y;
}

int garr[1] = {5};

int* alloc(int x) {
    static int arr[1];
    arr[0] = x;
    return arr;
}

int* alloc_pair(int x, int y) {
    static int arr[2];
    arr[0] = x;
    arr[1] = y;
    return arr;
}

int* alloc_offset(int x, int y) {
    static int arr[2];
    arr[0] = x;
    arr[1] = y;
    return arr + 1;
}

int** alloc_pointer(int x) {
    static int** p;
    static int* q;
    static int r;
    r = x;
    q = &r;
    p = &q;
    return p;
}