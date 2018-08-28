// EXPECT: 4

int main() {
    struct {
        int a;
    } x;
    return sizeof(x);
}