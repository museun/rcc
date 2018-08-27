// EXPECT: 42

int main() {
    int x   = 0;
    char* p = &x;
    p[0]    = 42;
    return x;
}