// EXPECT: 12

typedef int myint;

int main() {
    myint foo = 3;
    return sizeof(foo) * foo;
}