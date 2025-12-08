#include <stdio.h>
int main() {
    int x = 64;
    if (x == 32) {
        printf("x is %d\n", x);
    } else if (x == 64) {
        printf("x is %d\n", x);
    } else {
        printf("x is not 32 or 64\n");
    }

    return 0;
}
