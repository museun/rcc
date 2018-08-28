// EXPECT: 8

int main() {
    struct {
        char a;
        int b;
    } x;

    struct {
        char a;
        int b;
    }* p = &x;

    x.a = 3;
    x.b = 5;

    return p->a + p->b;
}