// EXPECT: 42

int main() {
    int** p = alloc_pointer(42);
    return **p;
}