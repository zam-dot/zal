@c {
    #include <stdio.h>
}

func main() {
    // Test various writes
    var f = fopen("test.txt", "w")
    
    // Different fprintf formats
    fprintf(f, "Integer: %d\n", 100)
    fprintf(f, "Float: %f\n", 3.14159)
    fprintf(f, "String: %s\n", "Hello")
    fprintf(f, "Char: %c\n", 'A')
    fprintf(f, "Hex: 0x%x\n", 255)
    
    fclose(f)
    
    printf("File written successfully!\n")
}
