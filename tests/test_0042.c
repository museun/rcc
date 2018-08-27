// EXPECT: 89

int main() {
    int i = 1;
    int j = 1;
    for (int k = 0; k < 10; k = k + 1) {
        int m = i + j;
        i     = j;
        j     = m;
    }
    return i;
}