// EXPECT: 45

int main() {
    int i = 0;
    int j = 0;
    while (i < 10) {
        j = j + i;
        i = i + 1;
    }
    return j;
}