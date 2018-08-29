// EXPECT: 8

int main() {
    struct {
        struct {
            int b;
            int c;
        } a;
    } x;

    x.a.b = 3;
    x.a.c = 5;

    return x.a.b + x.a.c;
}