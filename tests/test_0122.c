// EXPECT: 1

int main() {
    int i = 1;
    for (int i = 5; i < 10; i = i + 1)
        ;
    return i;
}