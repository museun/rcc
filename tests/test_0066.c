// EXPECT: 15

int x[5];
int main() {
    x[0] = 5;
    x[4] = 10;
    return x[0] + x[4];
}