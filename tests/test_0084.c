// EXPECT: 8

int main() {
    struct {
        char a;
        int b;
    } x;
    return sizeof(x);
}