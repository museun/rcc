// EXPECT: 8

int main() {
    struct tag {
        char a;
        int b;
    } x;

    struct tag* p = &x;

    x.a = 3;
    x.b = 5;

    return p->a + p->b;
}