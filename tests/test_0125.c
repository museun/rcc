// EXPECT: 1

int main() {
    int a[2];
    a[0]   = 1;
    a[1]   = 2;
    int* p = a;
    return *p++;
}