
#include <stdio.h>
#include <stdlib.h>
int main() {
    int    numbers[] = {10, 20, 30, 40, 50};
    double floats[] = {1.5, 2.5, 3.5};
    char  *strings[] = {"a", "b", "c"};
    int    first = numbers[0];
    int    third = numbers[2];
    numbers[1] = 99;
    printf("First: %d\n", first);
    printf("Third: %d\n", third);
    printf("Second: %d\n", numbers[1]);
    printf("float first: %lf\n", floats[0]);
    printf("strings first: %s\n", strings[0]);
    return 0;
}
