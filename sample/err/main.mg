@c {
    #include <stdio.h>
    #include <stdlib.h>
}

// Function that can return an error
func divide(a: int, b: int): (int, error) {
    if b == 0 {
        return 0, "division by zero"
    }
    return a / b, nil
}

func main() {
    result, err := divide(10, 2)
    if err != NULL {
        print("Error: %s\n", err)
    } else {
        print("Result: %d\n", result)  // Result: 5
    }

    result2, err2 := divide(10, 0)
    if err2 != NULL {
        print("Error: %s\n", err2)  // Error: division by zero
    }
}


