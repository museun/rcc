// EXPECT: 21

int sum(int a, int b, int c, int d, int e, int f) {
    return a + b + c + d + e + f;
}
int main() {
    return sum(1, 2, 3, 4, 5, 6);
}