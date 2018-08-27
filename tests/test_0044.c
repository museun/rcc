// EXPECT: 8

int main() {
    int* p = alloc_pair(3, 5);
    return *p + *(p + 1);
}