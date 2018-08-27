// EXPECT: 45

int main() {
    int x = 0;
    int y = 0;
    do {
        y = y + x;
        x = x + 1;
    } while (x < 10);
    return y;
}