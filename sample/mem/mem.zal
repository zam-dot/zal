@c {
    #include <stdio.h>
    #include <stdlib.h> 
}

func main() {
    var weights: double = getmem(2 * sizeof(float64))
    weights[0] = 1.23
    weights[1] = 4.56
    print("Value 0: %g\n", weights[0])
    print("Value 1: %g\n", weights[1])
    free(weights)
    
}
