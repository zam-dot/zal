@c {
    #include <stdio.h>
    #include <math.h>
}

const S_CONST = "contant"

func main() {
    print("%s\n", S_CONST)

    const n = 500000000

    print("%d\n", n)

    const d: float = 3e20 / n  // not parsing after /
    print("%g\n", d)

    print("%f\n", sin(d))    
}
