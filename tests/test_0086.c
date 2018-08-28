// EXPECT: 3

int main() {
    struct {
        char a;
    } x;

    x.a = 3;
    return x.a;
}