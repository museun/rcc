// EXPECT: 9

int main() {
    int* p = alloc_offset(2, 7);
    return *p + *(p - 1);
}