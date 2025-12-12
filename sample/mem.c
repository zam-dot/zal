
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main() {
    size_t ptr = (size_t)malloc(100);
    printf("Allocated: %zu\n", ptr);

    char *p = (char *)ptr;
    strcpy(p, "Hello World");
    printf("Stored: %s\n", p);
    free((void *)(ptr));
    return 0;
}
