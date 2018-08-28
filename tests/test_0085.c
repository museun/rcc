// EXPECT: 12

int main() {
    struct {
        char a;
        char b;
        int c;
        char d;
    } x;
    return sizeof(x);
}