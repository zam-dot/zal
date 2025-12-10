#include <stdio.h>
#define PI 3.14
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int mul(int a, int b) { return a * b; }
int div(int a, int b) { return a / b; }
int main() {
    printf("addition = %d\n", add(32, 2));
    printf("subtraction = %d\n", sub(32, 2));
    printf("multiplication = %d\n", mul(32, 2));
    printf("division = %d\n", div(32, 2));
    for (int i = 0; i < 10; i = i + 1) {
        printf("i = %d\n", i);
    }
    int i = 9;
    if ((i % 2 == 0)) {
        printf("i \\ 2 = %d\n", i % 2);
    } else {
        printf("i \\ 2 = %d\n", i % 2);
    }
    printf("number = %.2lf\n", PI);
    const char *myName = "HOla Pedor";
    printf("myName = %s\n", myName);
    return 0;
}
