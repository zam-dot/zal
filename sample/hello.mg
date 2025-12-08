@c {#include <stdio.h>

    int add(int a, int b) {
        return a + b;
    }
}

func main() {
    @c { int result = add(11, 22); }
    print("%d\n", result)
}
