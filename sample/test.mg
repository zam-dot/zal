@c {
#include <stdio.h>
#include <stdlib.h>
}

func main() {
    // Use char* for string arrays for now
    var numbers = [10, 20, 30, 40, 50]
    var floats = [1.5, 2.5, 3.5]
    var strings = ["a", "b", "c"]
    
    var first = numbers[0]
    var third = numbers[2]
    numbers[1] = 99
    
    print("First: %d\n", first)
    print("Third: %d\n", third)
    print("Second: %d\n", numbers[1])
    
    print("float first: %lf\n", floats[0])
    print("strings first: %s\n", strings[0])
}
