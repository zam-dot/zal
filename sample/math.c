#include <stdio.h>
#define PI 3.14159
typedef struct {
    int age;
    int weight;
} Person;

double divide(double x, double y) { return x / y; }
int    main() {
    Person p = {.age = 25, .weight = 42};
    if (p.age == 30) {
        printf("Age is 30\n");
    } else if (p.age == 25) {
        printf("Age is 25\n");
    } else {
        printf("Age is not 30 or 25\n");
    }

    printf("\nPI: %.2f\n\n", PI);
    double numss = (23 * 3) * (34 * 0.4);
    printf("numss: %.2f\n", numss);
    printf("Age: %d\n", p.age);
    printf("Weight: %d\n", p.weight);
    printf("\nFloat division: %.2f\n\n", divide(100.0, 3.0));
    int myNumbers[] = {1, 2, 3, 4, 5};
    for (int i = 0; i < 5; i = i + 1) {
        printf("Array[%d]: %d\n", i, myNumbers[i]);
    }
    return 0;
}
