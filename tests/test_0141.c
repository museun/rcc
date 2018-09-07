// EXPECT: 1

int main() {
    char* p = "abc"
              "def";

    // clang-format off
    char* p = "asdf" "ghjk";
    // clang-format on

    int i = 1;
    // this is a comment \
    int i = 0; and continues onto this line

    char* p = "asdf";

    // clang-format off
    char* z = "a"       "b"     
              "c" \
              "d";
    // clang-format on

    return i;
}