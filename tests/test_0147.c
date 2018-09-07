// EXPECT: 8

int main() {
    int a[2][2];
    a[0][0] = 3;
    a[1][0] = 5;
    return add_arr(a);
}