
#include <stdio.h>
int main() {
    int a = 20;
    int b = 4;
    int sum = a + b;
    int diff = a - b;
    int product = a * b;
    int quotient = a / b;
    printf("Sum: %d\n", sum);
    printf("Difference: %d\n", diff);
    printf("Product: %d\n", product);
    printf("Quotient: %d\n", quotient);
    if (product > 80) {
        printf("Product is greater than 80\n");
    } else if (quotient < 10) {
        printf("Quotient is less than 10\n");
    }

    return 0;
}
