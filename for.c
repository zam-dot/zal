
#include <stdio.h>
int main() {
    int arr[] = {100, 200, 300};
    for (int _i = 0; _i < sizeof(arr) / sizeof(arr[0]); _i++) {
        int i = _i;
        int v = arr[_i];
        printf("arr[%d] = %d\n", i, v);
    }
    int arr2[] = {10, 20, 30};
    for (int _i = 0; _i < sizeof(arr2) / sizeof(arr2[0]); _i++) {
        int v = arr2[_i];
        printf("value = %d\n", v);
    }
    for (int i = 0; i <= 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
