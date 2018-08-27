// EXPECT: 3

int main() {
    int arr[2];
    *arr       = 1;
    *(arr + 1) = 2;
    return *arr + *(arr + 1);
}