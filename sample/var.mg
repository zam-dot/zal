@c {
    #include <stdio.h>
    #include <stdlib.h>
}

func main() {
    // Clean typed allocation!
    ints := alloc(int, 10)      // int* ints = malloc(10 * sizeof(int))
    chars := alloc(char, 256)   // char* chars = malloc(256 * sizeof(char))
    
    defer free(ints)
    defer free(chars)
    
    // Use the memory
    ints[0] = 42;
    print("ints[0] = %d\n", ints[0]);
    
    // For strings, still need @c or add string functions
    @c {
        sprintf(chars, "Hello from alloc!");
        printf("%s\n", chars);
    }
    
    return 0
}
