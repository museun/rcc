// EXPECT: 8

int main() {
    struct {
        char a;
        char b;
    } x;
    x.a = 3;
    x.b = 5;
    return x.a + x.b;
}