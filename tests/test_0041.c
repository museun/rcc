// EXPECT: 60

int main() {
    int sum = 0;
    int i;
    for (i = 10; i < 15; i = i + 1)
        sum = sum + i;
    return sum;
}