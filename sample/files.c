
#include <stdio.h>
int main() {
    FILE *f = fopen("test.txt", "w");
    fprintf(f, "Integer: %d\n", 100);
    fprintf(f, "Float: %f\n", 3.14159);
    fprintf(f, "String: %s\n", "Hello");
    fprintf(f, "Hex: 0x%x\n", 255);
    fclose(f);
    printf("File written successfully!\n");
    return 0;
}
