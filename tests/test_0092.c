// EXPECT: 8

int main() {
    struct {
        struct {
            int b;
            int c[3];
        } a[2];
    } x;

    x.a[0].b    = 3;
    x.a[1].c[0] = 5;

    return x.a[0].b + x.a[1].c[0];
}