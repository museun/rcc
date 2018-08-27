// EXPECT: 3

int one() {
    return 1;
}
int two() {
    return 2;
}
int main() {
    return add(one(), two());
}