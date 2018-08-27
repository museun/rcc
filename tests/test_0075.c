// EXPECT: 42

// this is a comment

int main() {
    int x = 21; /* comment
                    here */

    /* and
int x = 43; * this
    * style
    */

    // int x = 44;

    int y = /* 22 */ 21;

    // int /* x */ y = 22 /* ?? */;

    // this should return 42
    return x + y; // this shouldn't be an error
}

// or this

///* what happens here?
