// EXPECT: 8

int add_array(int (*a)[2]) {
    return a[0][0] + a[1][0];
}

int main() {
    int b[2][2];
    b[0][0] = 3;
    b[1][0] = 5;
    return add_array(b);
}