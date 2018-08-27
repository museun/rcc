// EXPECT: 42

int main() {
    int* p = alloc(42);
    return *p;
}