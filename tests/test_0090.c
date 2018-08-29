// EXPECT: 48

int main() {
    struct {
        struct {
            int b;
            int c[5];
        } a[2]; // 24*2
    } x;

    return sizeof(x);
}