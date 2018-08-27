// EXPECT: 42

int main() {
    return 37 + ({ return 5; });
}