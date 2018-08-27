// EXPECT: 5

int main() {
    int x;
    int* p = &x;
    x      = 5;
    return p[0];
}