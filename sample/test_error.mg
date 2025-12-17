@c {
    #include <stdio.h>
    
    int add(int a, int b) {
        return a + b;
    }
}

func main() {
    var result: int = add(5, 3)
    print("5 + 3 = %d\n", result)
}
