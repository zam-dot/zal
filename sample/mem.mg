@c {
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
}

// func test() {
//     // Allocate memory
//     var ptr:int = getmem(100)
//
//     @c {
//         // Use it directly in C
//         char* p = (char*)ptr;
//         strcpy(p, "Test string");
//         printf("Allocated: %s\n", p);
//     }
//
//     // Free it
//     freemem(ptr)
// }

func main() {
    var ptr = getmem(100)
    print("Allocated: %zu\n", ptr)
    
    @c {
        char* p = (char*)ptr;
        strcpy(p, "Hello World");
    }
    print("Stored: %s\n", p)
    
    freemem(ptr)
}
